//! Any input must neither panic nor crash the process, and both interpreters (the
//! tree-walking one and the compiled closures) must give the same result.
#![no_main]

use libfuzzer_sys::fuzz_target;
use yes_lang::json::Json;
use yes_lang::{Context, Limits, Program, eth};

const TX: &str = include_str!("../../examples/tx.json");

fuzz_target!(|data: &[u8]| {
    let Ok(src) = std::str::from_utf8(data) else {
        return;
    };
    let Ok(program) = Program::compile(src) else {
        return;
    };
    // few enough calls to keep each run fast
    let limits = Limits {
        max_depth: 1_000,
        max_calls: Some(10_000),
    };
    let ctx = Context::default();
    eth::define(&ctx);
    ctx.bind("tx", Json::hex(TX)).unwrap();
    let compiled = program.run_once(ctx.fork(), limits);
    let tree = program.run_tree_walking(ctx.fork(), limits);
    // functions are compared by identity, so the results are compared by their debug output
    assert_eq!(
        format!("{:?}", compiled),
        format!("{:?}", tree),
        "the interpreters disagree on: {}",
        src
    );
    if let Ok(obj) = compiled {
        let _ = (obj.to_string(), yes_lang::json::to_json(&obj));
    }
    let _ = yes_lang::take_warnings();
});
