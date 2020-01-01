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
import chisel3._


/** Driver for AXI4-Stream
  *
  * Drives everything which gets appended to stimulus on the interface
  *
  */
class AxiStreamMaster(val iface: AxiStreamIf,
                      val peek: Bits => BigInt,
                      val poke: (Bits, BigInt) => Unit,
                      val println: String => Unit,
                      val ident: String = "") extends Bfm {
  private var stim = ListBuffer[AxiStreamBfmCycle]()
  private var verbose : Boolean = false

  def stimAppend(tdata: BigInt, tlast: BigInt, tuser: BigInt = 0): Unit = {
    stim += new AxiStreamBfmCycle(tdata, tlast, tuser)
  }

  def stimAppend(ls: List[AxiStreamBfmCycle]): Unit = {
    stim ++= ls
  }

  def setVerbose(enable_verbose: Boolean) {
    verbose = enable_verbose
  }

  private def printWithBg(s: String): Unit = {
    // black on orange
    println("\u001b[30;48;5;166m" + s + "\u001b[39;49m")
  }

  private object State extends Enumeration {
    type State = Value
    val Idle, Drive = Value
  }
  private var state = State.Idle
  private var ready: Boolean = peek(iface.tready) > 0

  private def driveInterfaceFromStimList(poke: (Bits, BigInt) => Unit): Unit = {
    val d = stim.remove(0)

    poke(iface.tvalid, 1)
    poke(iface.tdata, d.tdata)
    poke(iface.tlast, d.tlast)
    poke(iface.tuser, d.tuser)
  }

  private def releaseInterface(poke: (Bits, BigInt) => Unit): Unit = {
    poke(iface.tvalid, 0)
    poke(iface.tdata, 0)
    poke(iface.tlast, 0)
    poke(iface.tuser, 0)
  }

  def update(t: Long, poke: (Bits, BigInt) => Unit): Unit = {
    if (verbose) {
      printWithBg(f"${t}%5d AxiStreamMaster($ident): ready = ${ready}")
    }

    state match {
      case State.Idle => {
        if (stim.nonEmpty) {
          driveInterfaceFromStimList(poke)
          state = State.Drive
        }
      }
      case State.Drive => {
        if (ready) {
          val tdata = peek(iface.tdata)
          val tuser = peek(iface.tuser)
          val tlast = peek(iface.tlast)
          printWithBg(f"${t}%5d AxiStreamMaster($ident): sent tdata=${tdata}, tlast=${tlast}, tuser=${tuser}")

          if (stim.nonEmpty) {
            driveInterfaceFromStimList(poke)
            state = State.Drive
          } else {
            releaseInterface(poke)
            state = State.Idle
          }
        } else {
          // printWithBg(f"${t}%5d Driver: backpressure")
        }
      }
    }

    ready = peek(iface.tready) > 0
  }

  printWithBg(f"      AxiStreamMaster($ident): BFM initialized")
}
