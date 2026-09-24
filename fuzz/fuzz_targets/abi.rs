//! Decoding any data with any signature must neither panic nor crash (nor allocate more
//! than the data can hold): the first line of the input is the types, the rest is the data.
#![no_main]

use libfuzzer_sys::fuzz_target;
use yes_lang::{Context, Object, Program, eth};

fuzz_target!(|input: &[u8]| {
    let split = input
        .iter()
        .position(|b| *b == b'\n')
        .unwrap_or(input.len());
    let Ok(types) = std::str::from_utf8(&input[..split]) else {
        return;
    };
    let data = input.get(split + 1..).unwrap_or_default().to_vec();
    thread_local! {
        static PROGRAMS: (Program, Program) = (
            Program::compile("abi_decode(data, types)").unwrap(),
            Program::compile("abi(data, \"f(\" + types + \")\")").unwrap(),
        );
    }
    let ctx = Context::default();
    eth::define(&ctx);
    ctx.bind("types", Object::Str(types.to_string())).unwrap();
    ctx.bind("data", Object::Bytes(data.into())).unwrap();
    PROGRAMS.with(|(decode, abi)| {
        let _ = decode.run_once(ctx.fork(), Default::default());
        let _ = abi.run_once(ctx.fork(), Default::default());
    });
});
