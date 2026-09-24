//! Hex encoding: "0x..." strings, as used for bytes and numbers in Ethereum JSON-RPC,
//! in `b"..."` literals and by the builtins `int` and `bytes`.

#[cfg(feature = "json")]
use std::rc::Rc;

use num_bigint::BigInt;

#[cfg(feature = "json")]
use crate::eval::Bytes;

pub(crate) fn hex_digit(c: u8) -> Option<u8> {
    match c {
        b'0'..=b'9' => Some(c - b'0'),
        b'a'..=b'f' => Some(c - b'a' + 10),
        b'A'..=b'F' => Some(c - b'A' + 10),
        _ => None,
    }
}

/// Decodes an even number of hex digits (without '0x').
pub(crate) fn decode_hex(hex: &str) -> Option<Vec<u8>> {
    let hex = hex.as_bytes();
    if !hex.len().is_multiple_of(2) {
        return None;
    }
    hex.as_chunks::<2>()
        .0
        .iter()
        .map(|pair| Some(hex_digit(pair[0])? << 4 | hex_digit(pair[1])?))
        .collect()
}

/// Decodes a "0x..." hex string like `parse_bytes`, into a single allocation.
#[cfg(feature = "json")]
pub(crate) fn parse_shared_bytes(s: &str) -> Option<Bytes> {
    let hex = s.strip_prefix("0x")?.as_bytes();
    if !hex.iter().all(u8::is_ascii_hexdigit) {
        return None;
    }
    let digit = |c: u8| hex_digit(c).expect("validated");
    // an odd number of digits: the first byte has one digit
    let (first, rest) = hex.split_at(hex.len() % 2);
    // with an exact size known upfront, collecting allocates once
    let bytes: Rc<[u8]> = first
        .iter()
        .map(|&c| digit(c))
        .chain(
            rest.as_chunks::<2>()
                .0
                .iter()
                .map(|[hi, lo]| digit(*hi) << 4 | digit(*lo)),
        )
        .collect();
    Some(bytes.into())
}

/// Decodes a "0x..." hex string, an odd number of digits is padded with a leading zero
/// (numbers are written like that, e.g. "0x1").
pub(crate) fn parse_bytes(s: &str) -> Option<Vec<u8>> {
    let hex = s.strip_prefix("0x")?;
    if hex.len().is_multiple_of(2) {
        decode_hex(hex)
    } else {
        decode_hex(&format!("0{}", hex))
    }
}

pub(crate) fn parse_quantity(s: &str) -> Option<BigInt> {
    let hex = s.strip_prefix("0x")?;
    // from_str_radix and parse_bytes would also accept a sign or '_'
    if hex.is_empty() || !hex.bytes().all(|c| c.is_ascii_hexdigit()) {
        return None;
    }
    // fast path: most quantities (gas, nonce, ...) fit into 64 bits
    if hex.len() <= 16 {
        return u64::from_str_radix(hex, 16).ok().map(BigInt::from);
    }
    BigInt::parse_bytes(hex.as_bytes(), 16)
}

/// Encodes bytes as a "0x..." hex string.
pub(crate) fn encode(bytes: &[u8]) -> String {
    let mut hex = String::with_capacity(2 + bytes.len() * 2);
    hex.push_str("0x");
    for byte in bytes {
        hex.push_str(&format!("{:02x}", byte));
    }
    hex
}
