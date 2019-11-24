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


import scala.collection.mutable.ListBuffer
import scala.util.Random

import chisel3._


/** Monitor for AXI4-Stream
  *
  * Stores all received data in internal list
  *
  */
class AxiStreamSlave(val iface: AxiStreamIf,
                     val peek: Bits => BigInt,
                     val poke: (Bits, BigInt) => Unit,
                     val println: String => Unit,
                     val ident: String = "",
                     val rnd_gen: Random) extends Bfm {

  var backpressure : Double = 0.8
  private var resp = ListBuffer[(BigInt, BigInt)]()
  private var stop_next : Boolean = false
  private var verbose : Boolean = false

  private def printWithBg(s: String): Unit = {
    // dark blue on light gray
    println("\u001b[38;5;18;47m" + s + "\u001b[39;49m")
  }

  def respGet(): List[(BigInt, BigInt)] = {
    val ls = resp.result()
    resp.clear()
    ls
  }

  def setVerbose(enable_verbose: Boolean) {
    verbose = enable_verbose
  }

  def update(t: Long): Unit = {
    val rnd = Math.random()
    val ready_val = if (rnd > backpressure) { 0 } else { 1 }
    if (verbose) {
      printWithBg(f"${t}%5d Monitor($ident): ready = ${ready_val}")
    }
    poke(iface.tready, ready_val)

    if (stop_next) {
      stop_next = false
      throw new AxiStreamSlaveLastRecv(s"seen tlast at ${t-1}")
    }
    val vld = peek(iface.tvalid)
    if (vld != 0 && ready_val != 0) {
      val d = peek(iface.tdata)
      val u = peek(iface.tuser)
      val l = peek(iface.tlast)
      val last = peek(iface.tlast)
      if (last > 0) {
        printWithBg(f"${t}%5d Monitor($ident): seen TLAST")
        stop_next = true
      }
      resp += Tuple2(d, u)
      printWithBg(f"${t}%5d Monitor($ident): received tdata=${d}, tuser=${u}")
    }
  }
}
