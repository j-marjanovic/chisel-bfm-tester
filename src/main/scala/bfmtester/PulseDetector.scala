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

import chisel3._

import scala.collection.mutable.ListBuffer

class PulseDetector(
    val iface: Bool,
    val peek: Bits => BigInt,
    val poke: (Bits, BigInt) => Unit,
    val println: String => Unit,
    val ident: String = ""
) extends Bfm {

  class Pulse(val start: Long, val len: Long) {}

  private def printWithBg(s: String): Unit = {
    // dark red on light gray
    println("\u001b[38;5;124;47m" + s + "\u001b[39;49m")
  }

  private val pulses: ListBuffer[Pulse] = ListBuffer[Pulse]()
  private var pulse_start: Option[Long] = None

  def getPulses: List[Pulse] = {
    val lst = pulses.toList
    pulses.clear()
    lst
  }

  override def update(t: Long, poke: (Bits, BigInt) => Unit): Unit = {
    val v = peek(iface) > 0

    if (v && pulse_start.isEmpty) {
      pulse_start = Some(t)
      printWithBg(f"${t}%5d PulseDetector($ident): pulse start at ${t}")
    } else if (!v && pulse_start.nonEmpty) {
      pulses += new Pulse(start = pulse_start.get, len = t - pulse_start.get)
      pulse_start = None
      printWithBg(f"${t}%5d PulseDetector($ident): pulse finish at ${t}")
    }
  }

  printWithBg(f"      PulseDetector($ident): BFM initialized")
}
