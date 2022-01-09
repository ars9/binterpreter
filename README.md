# Development

Installing dependencies

```sh
cargo update
```

Building project

```sh
cargo build
```

Testing project

```sh
cargo test
```

# Usage

To compile sources to binaries, run this:

```sh
./target/debug/binterpreter compile ./samples/case_1.txt > /tmp/case_1.bin
./target/debug/binterpreter compile ./samples/loop_test.txt > /tmp/loop_test.bin
```

To execute binaries, run the following:

```sh
./target/debug/binterpreter exec ./samples/basic_add.bin
./target/debug/binterpreter exec ./samples/case_1.bin
./target/debug/binterpreter exec ./samples/loop_test.bin
```
./target/debug/binterpreter compile ./samples/loop_test.txt > /tmp/loop_test.bin
