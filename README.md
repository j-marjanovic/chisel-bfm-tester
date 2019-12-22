# BFM Tester for Chisel HDL

For more information please see the documentation in
[src/main/scala/bfmtester/package.scala](src/main/scala/bfmtester/package.scala).

Examples are available in [bfm-tester-examples](https://github.com/j-marjanovic/chisel-bfm-tester-examples).

## Changelog

### 0.3 - 2019-12-22

- Change poke behavior: now all pokes are put into a queue and the values
  are updated at end of each clock cycle - similar to Verilog's Non-Blocking
  Assignments
- AXI4-Stream slave BFM: Fix backpressure generation; random value is now
  generated from a testbench-wide random generator

### 0.2 - 2019-11-24

- Add basic infrastructure to call update()
- Add several BFMs (AXI4-Stream master and slave, AXI4-Lite master)

### 0.1 - 2019-11-24

- Initial commit
