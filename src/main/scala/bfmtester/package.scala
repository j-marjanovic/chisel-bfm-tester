/*
MIT License

Copyright (c) 2018-2019 Jan Marjanovic

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/**
 * =BFM Tester for Chisel HDL=
 *
 * ==Motivation==
 *
 * Chisel HDL is a Scala-based Domain Specific Language (DSL) for designing
 * hardware. One might see it as an evolution of classic Hardware Description
 * Languages (HDLs), such as VHDL and Verilog.
 *
 * One advantage of classic HDLs is the fact that both the design and the
 * test for it are written in the same language. This allows implementing
 * interfaces which are close to th
 *
 * In Chisel, although both the design and tests for it are written in the
 * same language (Scala), the constructs which can be used are vastly different
 * between the two parts. This is especially limiting when multiple input
 * or output ports of the Design Under Test (DUT) need to be driven on
 * monitored.
 *
 * ==Introduction==
 *
 * BFM Tester for Chisel HDL provides a new way to structure test harness for
 * Chisel modules. The module under test is connected to several Bus Functional
 * Model which monitor or drive its corresponding port every clock cycle.
 * This gives more flexibility that `OrderedDecoupledHWIOTester` from
 * chisel-testers. 
 *
 * ==Details==
 *
 * TODO
 *
 * ===Interfaces===
 *
 *  - [[AxiLiteIf]] - AXI4-Lite
 *  - [[AxiStreamIf]] - AXI4-Stream
 *
 * ===Bus Functional Models===
 *
 *  - [[AxiLiteMaster]] - AXI4-Lite master
 *  - [[AxiStreamMaster]] - AXI4-Stream master
 *  - [[AxiStreamSlave]] - AXI4-Stream slave
 *
 * ==Example==
 *
 * TODO
 *
 */

package object bfmtester {
}
