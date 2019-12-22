# BFM Tester for Chisel HDL

For more information please see the documentation in
[src/main/scala/bfmtester/package.scala](src/main/scala/bfmtester/package.scala).

Examples are available in [bfm-tester-examples](https://github.com/j-marjanovic/chisel-bfm-tester-examples).

## Changelog

### Unreleased

- AXI4-Stream slave BFM: Fix backpressure generation; random value is now
  generated from a testbench-wide random generator

### 0.2 - 2019-11-24

- Add basic infrastructure to call update()
- Add several BFMs (AXI4-Stream master and slave, AXI4-Lite master)

### 0.1 - 2019-11-24

- Initial commit
