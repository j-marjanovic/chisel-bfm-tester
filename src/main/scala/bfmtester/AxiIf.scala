/*
MIT License

Copyright (c) 2018-2020 Jan Marjanovic

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

import chisel3.util._
import chisel3._

class AxiIf(val addr_w: chisel3.internal.firrtl.Width,
            val data_w: chisel3.internal.firrtl.Width,
            val id_w: chisel3.internal.firrtl.Width)
    extends Bundle {
  val AW = Irrevocable(new Bundle {
    val id: UInt = UInt(id_w)
    val addr: UInt = UInt(addr_w)
    val len: UInt = UInt(8.W)
    val size: UInt = UInt(3.W)
    val burst: UInt = UInt(2.W)
    val lock: Bool = Bool()
    val cache: UInt = UInt(4.W)
    val prot: UInt = UInt(3.W)
    val qos: UInt = UInt(4.W)
    // val user: UInt = UInt(awuser_w)
    // region signal is optional
  })
  val W = Irrevocable(new Bundle {
    val id: UInt = UInt(id_w)
    val data: UInt = UInt(data_w)
    val strb: UInt = UInt((data_w.get / 8).W)
    val last: Bool = Bool()
    // val user: UInt = UInt(wuser_w)
  })
  val B = Flipped(Irrevocable(new Bundle {
    val id: UInt = UInt(id_w)
    val resp: UInt = UInt(3.W)
    // val user: UInt = UInt(buser_w)
  }))
  val AR = Irrevocable(new Bundle {
    val id: UInt = UInt(id_w)
    val addr: UInt = UInt(addr_w)
    val len: UInt = UInt(8.W)
    val size: UInt = UInt(3.W)
    val burst: UInt = UInt(2.W)
    val lock: Bool = Bool()
    val cache: UInt = UInt(4.W)
    val prot: UInt = UInt(3.W)
    val qos: UInt = UInt(4.W)
    // val user: UInt = UInt(aruser_w)
    // region signal is optional
  })
  val R = Flipped(Irrevocable(new Bundle {
    val id: UInt = UInt(id_w)
    val data: UInt = UInt(data_w)
    val resp: UInt = UInt(3.W)
    val last: Bool = Bool()
    // val user: UInt = UInt(ruser_w)
  }))
}
