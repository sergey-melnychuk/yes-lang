//! Any input must be converted from JSON the same way directly and through
//! `serde_json::Value`, and converting the result back to JSON and again from it
//! must give the same object.
#![no_main]

use libfuzzer_sys::fuzz_target;
use yes_lang::json::{Strings, from_json, parse_json, to_json};

fuzz_target!(|data: &[u8]| {
    let Ok(text) = std::str::from_utf8(data) else {
        return;
    };
    let value = serde_json::from_str::<serde_json::Value>(text);
    for strings in [Strings::Plain, Strings::Hex] {
        let direct = parse_json(text, strings);
        match (&direct, &value) {
            (Ok(direct), Ok(value)) => {
                assert_eq!(direct, &from_json(value, strings), "differ on: {}", text);
                // infinite floats cannot be converted back
                if let Ok(json) = to_json(direct) {
                    assert_eq!(
                        &from_json(&json, strings),
                        direct,
                        "no roundtrip for: {}",
                        text
                    );
                }
            }
            (Err(_), Err(_)) => {}
            _ => panic!(
                "only one way failed: {:?} / {:?} on: {}",
                direct.is_ok(),
                value.is_ok(),
                text
            ),
        }
    }
});
