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


package bfmtester


import chisel3._

/** AXI Stream interface, as defined in "AMBA 4 AXI4-Stream Protocol"
 *
 * @group interfaces
 */
class AxiStreamIf(val data_width: chisel3.internal.firrtl.Width,
                  val user_width: Int = 1) extends Bundle {
  val tdata = Input(UInt(data_width))
  val tvalid = Input(Bool())
  val tready = Output(Bool())
  val tlast = Input(Bool())
  val tuser = Input(UInt(user_width.W))
}

class AxiStreamBfmCycle(var tdata : BigInt,
                        var tlast : BigInt,
                        var tuser : BigInt
                       )

class AxiStreamSlaveLastRecv(msg: String = "") extends Exception(msg)
