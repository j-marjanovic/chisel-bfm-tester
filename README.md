# BFM Tester for Chisel HDL

For more information please see the documentation in
[src/main/scala/bfmtester/package.scala](src/main/scala/bfmtester/package.scala).

Examples are available in [bfm-tester-examples](https://github.com/j-marjanovic/chisel-bfm-tester-examples).

## Changelog

### 0.4.2 - 2021-11-27

- AXI4-Lite subordinate generator correctly handles `WSTRB` signal

### 0.4.1 - 2021-11-21

- AXI4-Lite subordinate generator can also generate C and C++ headers

### 0.4.0 - 2021-07-21

- Add AXI4 memory subordinate BFM
- Add Avalon® Memory-Mapped memory subordinate BFM
- Add Pulse Detector
- Add AXI4-Lite subordinate generator utility
- Add AxPROT to AXI4-Lite interface
- Upgrade to Chisel 3.4 and iotesters 1.5

### 0.3.1 - 2020-01-01

- AXI4-Stream manager BFM: add verbosity control
- AXI4-Stream subordinate BFM: update handling of the backpressure

### 0.3 - 2019-12-22

- Change poke behavior: now all pokes are put into a queue and the values
  are updated at end of each clock cycle - similar to Verilog's Non-Blocking
  Assignments
- AXI4-Stream subordinate BFM: Fix backpressure generation; random value is now
  generated from a testbench-wide random generator

### 0.2 - 2019-11-24

- Add basic infrastructure to call update()
- Add several BFMs (AXI4-Stream manager and subordinate, AXI4-Lite manager)

### 0.1 - 2019-11-24

- Initial commit

---

Arm and AMBA are registered trademarks of Arm Limited (or its subsidiaries) in
the US and/or elsewhere. Avalon is a registered trademark of Intel Corporation
or its subsidiaries. All trademarks are the property of their respective owners.
