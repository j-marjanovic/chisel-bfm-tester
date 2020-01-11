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
import chiseltest._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Chisel tester with Bus Functional Models */
trait BfmTester {
  this: ChiselScalatestTester =>
  private val bfms: ListBuffer[Bfm] = new ListBuffer[Bfm]()

  /** add Bfm to the list of Bfms to be executed each clock cycle */
  def Bfm[BfmT <: Bfm](bfm: BfmT): BfmT = {
    bfms += bfm
    bfm
  }

  private val update_queue = new mutable.Queue[(Bits, BigInt)]

  private def poke_onto_queue(signal: Bits, value: BigInt): Unit = {
    update_queue.enqueue((signal, value))
  }

  private def stepSingle(): Unit = {
    for (bfm <- bfms.toList) {
      bfm.update(t, poke_onto_queue)
    }
    for (el <- update_queue) {
      el._1 match {
        case b: Bool => el._1.poke(el._2.B)
        case u: UInt => el._1.poke(el._2.U)
        case _       => throw new UnsupportedOperationException("")
      }
    }
    update_queue.clear()
  }

  /** keeps track of the clock cycles elapsed */
  private var t: Int = 0

  implicit class testableClock(x: Clock) {
    def step(cycles: Int = 1): Unit = {
      for(_ <- 0 until cycles) {
        stepSingle()
        chiseltest.testableClock(x).step(cycles)
        t += 1
      }
    }
  }

  /** provide deterministic random number generator */
  private val rnd = scala.util.Random
  def rndSetSeed: Long => Unit = rnd.setSeed

  object BfmFactory {
    def create_axis_master(iface: AxiStreamIf, ident: String = ""): AxiStreamMaster = {
      Bfm(new AxiStreamMaster(iface, ident))
    }
    def create_axis_slave(iface: AxiStreamIf, ident: String = ""): AxiStreamSlave = {
      Bfm(new AxiStreamSlave(iface, ident, rnd))
    }
    def create_axilite_master(iface: AxiLiteIf, ident: String = ""): AxiLiteMaster = {
      Bfm(new AxiLiteMaster(iface))
    }
  }
}
