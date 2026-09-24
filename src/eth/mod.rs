//! Ethereum: Keccak-256, function selectors, ABI decoding of calldata and event logs.
//!
//! | native | |
//! |---|---|
//! | `keccak(x)` | Keccak-256 of bytes or of a UTF-8 string |
//! | `selector(signature)` | `keccak(signature)[..4]` |
//! | `abi(calldata, "transfer(address to, uint256 amount)")` | the arguments of a call, `null` if the selector differs |
//! | `abi_decode(data, "(address,uint256)")` | values encoded with the ABI |
//! | `event(log, "Transfer(address indexed from, address indexed to, uint256 value)")` | the values of an event, `null` if the log is another event |
//!
//! Values with names in the signature are decoded into a record, without names into a list.
//! Addresses and `bytes<N>` are decoded into bytes, integers of any size into integers.
//!
//! ```
//! use yes_lang::{Context, Object, Program, eth, json::Json};
//!
//! let ctx = Context::default();
//! eth::define(&ctx);
//! // Ethereum JSON-RPC writes bytes as "0x..." hex strings
//! ctx.bind("tx", Json::hex(r#"{"input": "0xa9059cbb"}"#))?;
//! let program = Program::compile(r#"tx.input[..4] == selector("transfer(address,uint256)")"#)?;
//! assert_eq!(program.run(&ctx)?, Object::Bool(true));
//! # Ok::<(), yes_lang::Error>(())
//! ```

mod abi;

use sha3::{Digest, Keccak256};

use crate::error::EvalError;
use crate::eval::{Context, Object, get_field};
use crate::native::Native;

use abi::{Param, Type};

/// Binds the Ethereum natives in the context.
pub fn define(ctx: &Context) {
    for native in natives() {
        ctx.define(native);
    }
}

/// `keccak`, `selector`, `abi`, `abi_decode`, `event`.
pub fn natives() -> Vec<Native> {
    vec![
        // Keccak-256 of bytes or of a UTF-8 string
        Native::pure("keccak", 1, |args| match &args[0] {
            Object::Bytes(b) => Ok(Object::Bytes(keccak(b).into())),
            Object::Str(s) => Ok(Object::Bytes(keccak(s.as_bytes()).into())),
            arg => Err(EvalError::Argument("keccak", arg.clone())),
        }),
        // function selector: first 4 bytes of keccak of the signature
        Native::pure("selector", 1, |args| match &args[0] {
            Object::Str(s) => Ok(Object::Bytes(keccak(s.as_bytes())[..4].to_vec().into())),
            arg => Err(EvalError::Argument("selector", arg.clone())),
        }),
        Native::pure("abi", 2, |args| {
            let (data, text) = (bytes("abi", &args[0])?, string("abi", &args[1])?);
            let signature = abi::signature(text).map_err(|e| EvalError::Native("abi", e))?;
            if data.len() < 4 || data[..4] != signature.hash[..4] {
                return Ok(Object::Null);
            }
            let params: Vec<&Param> = signature.params.iter().collect();
            decode("abi", &params, &data[4..])
        }),
        Native::pure("abi_decode", 2, |args| {
            let (data, text) = (
                bytes("abi_decode", &args[0])?,
                string("abi_decode", &args[1])?,
            );
            let params = abi::params(text).map_err(|e| EvalError::Native("abi_decode", e))?;
            let params: Vec<&Param> = params.iter().collect();
            decode("abi_decode", &params, data)
        }),
        Native::pure("event", 2, |args| {
            let log = match &args[0] {
                Object::Record(log) => log,
                arg => return Err(EvalError::Argument("event", arg.clone())),
            };
            let text = string("event", &args[1])?;
            let signature = abi::signature(text).map_err(|e| EvalError::Native("event", e))?;
            event(
                &signature,
                &get_field(log, "topics"),
                &get_field(log, "data"),
            )
        }),
    ]
}

fn bytes<'a>(name: &'static str, arg: &'a Object) -> Result<&'a [u8], EvalError> {
    match arg {
        Object::Bytes(b) => Ok(b),
        arg => Err(EvalError::Argument(name, arg.clone())),
    }
}

fn string<'a>(name: &'static str, arg: &'a Object) -> Result<&'a str, EvalError> {
    match arg {
        Object::Str(s) => Ok(s),
        arg => Err(EvalError::Argument(name, arg.clone())),
    }
}

fn decode(name: &'static str, params: &[&Param], data: &[u8]) -> Result<Object, EvalError> {
    let types: Vec<&Type> = params.iter().map(|param| &param.ty).collect();
    let values = abi::decode_tuple(&types, data).map_err(|e| EvalError::Native(name, e))?;
    Ok(abi::values(params, values))
}

// indexed values are in the topics (after the hash of the signature), the others in the data;
// a value of a dynamic type is indexed as its hash
fn event(signature: &abi::Signature, topics: &Object, data: &Object) -> Result<Object, EvalError> {
    let topics: Vec<&[u8]> = match topics {
        Object::List(topics) => topics
            .iter()
            .map(|topic| bytes("event", topic))
            .collect::<Result<_, _>>()?,
        _ => return Ok(Object::Null),
    };
    let indexed = signature
        .params
        .iter()
        .filter(|param| param.indexed)
        .count();
    // another event, or the same signature with other values indexed (ERC-20 and ERC-721)
    if topics.first() != Some(&&signature.hash[..]) || topics.len() != 1 + indexed {
        return Ok(Object::Null);
    }
    let failed = |e| EvalError::Native("event", e);
    let data = match data {
        Object::Bytes(data) => &data[..],
        Object::Null => &[],
        data => return Err(EvalError::Argument("event", data.clone())),
    };
    let unindexed: Vec<&Type> = signature
        .params
        .iter()
        .filter(|param| !param.indexed)
        .map(|param| &param.ty)
        .collect();
    let mut unindexed = abi::decode_tuple(&unindexed, data)
        .map_err(failed)?
        .into_iter();
    let mut topics = topics[1..].iter();
    let mut values = Vec::with_capacity(signature.params.len());
    for param in &signature.params {
        values.push(if !param.indexed {
            unindexed.next().unwrap()
        } else if abi::is_dynamic(&param.ty) {
            Object::Bytes(topics.next().unwrap().to_vec().into())
        } else {
            abi::decode(&param.ty, topics.next().unwrap()).map_err(failed)?
        });
    }
    let params: Vec<&Param> = signature.params.iter().collect();
    Ok(abi::values(&params, values))
}

pub(crate) fn keccak(data: &[u8]) -> Vec<u8> {
    Keccak256::digest(data).to_vec()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Program;
    use crate::eval::Bytes;

    // test vectors: encoded with eth-abi 6.0.0 (Python)
    const F: &str = "8be6524600000000000000000000000000000000000000000000000000000000000001230000000000000000000000000000000000000000000000000000000000000080313233343536373839300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000004560000000000000000000000000000000000000000000000000000000000000789000000000000000000000000000000000000000000000000000000000000000d48656c6c6f2c20776f726c642100000000000000000000000000000000000000";
    const G: &str = "2289b18c000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000001400000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000030000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000e000000000000000000000000000000000000000000000000000000000000000036f6e650000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000374776f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000057468726565000000000000000000000000000000000000000000000000000000";
    const H: &str = "bde3d15b000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000000200000000000000000000000011111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000005000000000000000000000000222222222222222222222222222222222222222200000000000000000000000000000000000000000000000000000000000000060000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000026869000000000000000000000000000000000000000000000000000000000000";
    const I: &str = "955c1f5cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007fffff00000000000000000000000000000000000000000000000000000000000000ff";
    const J: &str = "69b4b53d00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000300000000000000000000000000000000000000000000000000000000000000a00101010101010101010101010101010101010101010101010101010101010101000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000001610000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000026263000000000000000000000000000000000000000000000000000000000000";
    const K: &str = "26cfd10700000000000000000000000000000000000000000000000000000000000000070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000dededededededededededededededededededede00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000";

    fn bytes(hex: &str) -> Object {
        Object::Bytes(Bytes::from(crate::hex::decode_hex(hex).unwrap()))
    }

    // runs with both interpreters, with 'data' bound
    fn run(src: &str, data: Object) -> Result<Object, EvalError> {
        let program = Program::compile(src).unwrap();
        let ctx = Context::default();
        define(&ctx);
        ctx.put("data".to_string(), data);
        let compiled = program.run_once(ctx.fork(), Default::default());
        let tree = program.run_tree_walking(ctx.fork(), Default::default());
        assert_eq!(format!("{:?}", compiled), format!("{:?}", tree), "{}", src);
        compiled.map_err(EvalError::without_position)
    }

    fn check(src: &str, data: &str) {
        assert_eq!(run(src, bytes(data)), Ok(Object::Bool(true)), "{}", src);
    }

    #[test]
    fn test_keccak_and_selector() {
        let t = |src| check(src, "");
        t("selector(\"transfer(address,uint256)\") == b\"a9059cbb\"");
        t("keccak(\"\") == b\"c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470\"");
        t("keccak(b\"\") == keccak(\"\")");
        t(
            "keccak(\"Transfer(address,address,uint256)\") == b\"ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"",
        );
        assert_eq!(
            run("selector(b\"00\")", Object::Null),
            Err(EvalError::Argument("selector", bytes("00")))
        );
    }

    #[test]
    fn test_abi() {
        // the example of the Solidity documentation
        check(
            "let r = abi(data, \"f(uint256 a, uint32[] b, bytes10 c, bytes d)\");
             r == {a: 291, b: [1110, 1929], c: bytes(\"1234567890\"), d: bytes(\"Hello, world!\")}",
            F,
        );
        // without names: a list
        check(
            "abi(data, \"f(uint256, uint32[], bytes10, bytes)\") == [291, [1110, 1929], bytes(\"1234567890\"), bytes(\"Hello, world!\")]",
            F,
        );
        // nested dynamic arrays
        check(
            "abi(data, \"g(uint256[][], string[])\") == [[[1, 2], [3]], [\"one\", \"two\", \"three\"]]",
            G,
        );
        // tuples, named components are records
        check(
            "let r = abi(data, \"h((address to, uint256 n)[] xs, (string, bool) y)\");
             r.xs == [{to: b\"1111111111111111111111111111111111111111\", n: 5}, {to: b\"2222222222222222222222222222222222222222\", n: 6}] && r.y == [\"hi\", true]",
            H,
        );
        // signed integers and their bounds
        check(
            "abi(data, \"i(int8, int256, int24, uint8)\") == [-1, -57896044618658097711785492504343953926634992332820282019728792003956564819968, 8388607, 255]",
            I,
        );
        // fixed-size arrays, of a dynamic type too
        check(
            "abi(data, \"j(uint8[3], string[2], bytes32)\") == [[1, 2, 3], [\"a\", \"bc\"], b\"0101010101010101010101010101010101010101010101010101010101010101\"]",
            J,
        );
        check(
            "abi(data, \"k((uint256, bool), address, bytes)\") == [[7, false], b\"dededededededededededededededededededede\", b\"\"]",
            K,
        );
        // another selector: null
        check("abi(data, \"g(uint256[][], string[])\") == null", F);
        check(
            "abi(b\"\", \"f()\") == null && abi(b\"26121ff0\", \"f()\") == []",
            "",
        );
        // types of Solidity: 'uint' is 'uint256', data locations, 'tuple(...)'
        check(
            "abi(data, \"k(tuple(uint, bool) a, address b, bytes memory c)\") == {a: [7, false], b: b\"dededededededededededededededededededede\", c: b\"\"}",
            K,
        );
    }

    #[test]
    fn test_abi_decode() {
        let data = &F[8..];
        check(
            "abi_decode(data, \"(uint256, uint32[], bytes10, bytes)\")[1] == [1110, 1929]",
            data,
        );
        check(
            "abi_decode(data, \"uint256 a, uint32[] b, bytes10 c, bytes d\").a == 291",
            data,
        );
        check("abi_decode(data[..32], \"uint16\") == [291]", data);
    }

    #[test]
    fn test_invalid_data() {
        let fails = |src: &str, data: &str, message: &str| match run(src, bytes(data)) {
            Err(EvalError::Native(_, e)) => assert!(e.contains(message), "{}: {}", src, e),
            result => panic!("{}: {:?}", src, result),
        };
        let word = |hex: &str| format!("{:0>64}", hex);
        fails("abi_decode(data, \"uint256\")", "00", "too short");
        fails(
            "abi_decode(data, \"address\")",
            &word("01".repeat(21).as_str()),
            "invalid address",
        );
        fails("abi_decode(data, \"bool\")", &word("02"), "invalid bool");
        fails(
            "abi_decode(data, \"uint8\")",
            &word("0100"),
            "invalid uint8",
        );
        fails("abi_decode(data, \"int8\")", &word("80"), "invalid int8");
        fails(
            "abi_decode(data, \"bytes1\")",
            &"0101".repeat(16),
            "invalid bytes1",
        );
        fails(
            "abi_decode(data, \"string\")",
            &format!("{}{}{}", word("20"), word("01"), "ff".repeat(32)),
            "not UTF-8",
        );
        // offsets and lengths out of the data, huge lengths
        fails("abi_decode(data, \"bytes\")", &word("40"), "out of bounds");
        fails(
            "abi_decode(data, \"bytes\")",
            &format!("{}{}", word("20"), "ff".repeat(32)),
            "out of bounds",
        );
        fails(
            "abi_decode(data, \"uint256[]\")",
            &format!("{}{}", word("20"), word("10")),
            "too short for 16 items",
        );
        fails(
            "abi_decode(data, \"uint256[1000000000]\")",
            "",
            "too short for 1000000000 items",
        );
        // invalid signatures
        let invalid = |src: &str, message: &str| match run(src, Object::Null) {
            Err(EvalError::Native(_, e)) => assert!(e.contains(message), "{}: {}", src, e),
            result => panic!("{}: {:?}", src, result),
        };
        invalid("abi(b\"\", \"f(uint7)\")", "unknown type 'uint7'");
        invalid("abi(b\"\", \"f(bytes33)\")", "unknown type 'bytes33'");
        invalid("abi(b\"\", \"(uint256)\")", "expected a name");
        invalid("abi(b\"\", \"f(uint256\")", "expected ',' or ')'");
        invalid("abi(b\"\", \"f(uint256[0])\")", "invalid array size");
        // found by fuzzing: an empty tuple takes no space, so 10^10 of them "fit" into no data
        invalid("abi_decode(b\"\", \"(()[10000000000])\")", "empty tuple");
        invalid("abi_decode(b\"\", \"(tuple())\")", "empty tuple");
        invalid(
            &format!(
                "abi(b\"\", \"f({}uint{})\")",
                "(".repeat(40),
                ")".repeat(40)
            ),
            "nested too deep",
        );
    }

    #[test]
    fn test_event() {
        let t = |src: &str| assert_eq!(run(src, Object::Null), Ok(Object::Bool(true)), "{}", src);
        let transfer = "let log = {topics: [b\"ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\", b\"0000000000000000000000001111111111111111111111111111111111111111\", b\"0000000000000000000000002222222222222222222222222222222222222222\"], data: b\"000000000000000000000000000000000000000000000000000000000000002a\"};";
        let with_transfer = |src: &str| t(&format!("{} {}", transfer, src));
        with_transfer(
            "event(log, \"Transfer(address indexed from, address indexed to, uint256 value)\") == {from: b\"1111111111111111111111111111111111111111\", to: b\"2222222222222222222222222222222222222222\", value: 42}",
        );
        with_transfer(
            "event(log, \"Transfer(address indexed, address indexed, uint256)\") == [b\"1111111111111111111111111111111111111111\", b\"2222222222222222222222222222222222222222\", 42]",
        );
        // another event, or the same one with other values indexed (ERC-721: the id too)
        with_transfer(
            "event(log, \"Approval(address indexed a, address indexed b, uint256 c)\") == null",
        );
        with_transfer(
            "event(log, \"Transfer(address indexed a, address indexed b, uint256 indexed c)\") == null",
        );
        t("event({topics: [], data: b\"\"}, \"E()\") == null && event({}, \"E()\") == null");
        // an indexed value of a dynamic type is its hash
        t("let log = {topics: [b\"d8c0abd67fe7265e8f52f01f15806092fe956720ef6adf58c8b3cd72a813fcfc\", b\"1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8\"], data: b\"000000000000000000000000000000000000000000000000000000000000002a00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000002abcd000000000000000000000000000000000000000000000000000000000000\"};
             event(log, \"Named(string indexed name, uint256 n, bytes b)\") == {name: keccak(\"hello\"), n: 42, b: b\"abcd\"}");
    }
}
