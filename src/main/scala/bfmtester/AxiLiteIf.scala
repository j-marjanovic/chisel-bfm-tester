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
import chisel3.util.Irrevocable

/** AXI4-Lite interface, as defined in "AMBA AXI4-Lite Interface Specification"
 *
 * @group interfaces
 */
class AxiLiteIf(val addr_w: chisel3.internal.firrtl.Width,
                val data_w: chisel3.internal.firrtl.Width = 32.W) extends Bundle {
  val AW = Flipped(Irrevocable(new Bundle {
    val addr: UInt = UInt(addr_w)
    val prot: UInt = UInt(3.W)
  }))
  val W = Flipped(Irrevocable(new Bundle {
    val wdata = UInt(data_w)
    val wstrb = UInt((data_w.get/8).W)
  }))
  val B = Irrevocable(UInt(2.W))
  val AR = Flipped(Irrevocable(new Bundle {
    val addr: UInt = UInt(addr_w)
    val prot: UInt = UInt(3.W)
  }))
  val R = Irrevocable(new Bundle {
    val rdata = UInt(data_w); val rresp = UInt(2.W)
  })
}
