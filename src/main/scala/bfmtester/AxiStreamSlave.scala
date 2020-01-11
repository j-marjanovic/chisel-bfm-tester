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
import chiseltest._

/** Monitor for AXI4-Stream
  *
  * Stores all received data in internal list
  *
  */
class AxiStreamSlave(val iface: AxiStreamIf,
                     val ident: String = "",
                     val rnd_gen: Random) extends Bfm {

  var backpressure : Double = 0.2
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

  def update(t: Long, poke: (Bits, BigInt) => Unit): Unit = {
    val rnd = rnd_gen.nextDouble()
    val ready_val = if (rnd > backpressure) { 1 } else { 0 }
    if (verbose) {
      printWithBg(f"${t}%5d AxiStreamSlave($ident): ready = ${ready_val}")
    }

    poke(iface.tready, ready_val)

    if (stop_next) {
      stop_next = false
      throw new AxiStreamSlaveLastRecv(s"seen tlast at ${t-1}")
    }
    val vld = iface.tvalid.peek()
    if (vld != 0 && ready_val != 0) {
      val d = iface.tdata.peek()
      val u = iface.tuser.peek()
      val l = iface.tlast.peek()
      if (l.litValue() > 0) {
        printWithBg(f"${t}%5d AxiStreamSlave($ident): seen TLAST")
        stop_next = true
      }
      resp += Tuple2(d.litValue(), u.litValue())
      printWithBg(f"${t}%5d AxiStreamSlave($ident): received tdata=${d}, tuser=${u}")
    }
  }

  printWithBg(f"      AxiStreamSlave($ident): BFM initialized")
}
