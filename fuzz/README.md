Fuzzing
=======

Requires a nightly toolchain and [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz)
(`cargo install cargo-fuzz`).

Targets:
- `run`: any input must neither panic nor crash, and both interpreters (tree-walking and
  compiled closures) must give the same result;
- `json`: JSON must be converted the same way directly and through `serde_json::Value`,
  and survive a roundtrip back to JSON.

```shell
cd fuzz
# seed corpora: the examples, some transactions
mkdir -p corpus/run corpus/json
cp ../examples/*.yes ../examples/errors/*.yes corpus/run/
cp ../examples/tx.json ../examples/receipt.json corpus/json/
cargo +nightly fuzz run run -- -dict=yes.dict -max_len=2048 -max_total_time=300
cargo +nightly fuzz run json -- -max_len=4096 -max_total_time=300
```
