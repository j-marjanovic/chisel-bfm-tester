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
import chisel3.experimental.MultiIOModule
import chisel3.iotesters.PeekPokeTester

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Chisel tester with Bus Functional Models */
abstract class BfmTester[+T <: MultiIOModule](dut: T) extends PeekPokeTester(dut) {
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
      poke(el._1, el._2)
    }
    update_queue.clear()
    super.step(1)
  }

  override def step(n: Int): Unit = {
    for(_ <- 0 until n) {
      stepSingle()
    }
  }

  object BfmFactory {
    def create_axis_master(iface: AxiStreamIf, ident: String = ""): AxiStreamMaster = {
      Bfm(new AxiStreamMaster(iface, peek, poke, println, ident))
    }

    def create_axis_slave(iface: AxiStreamIf, ident: String = ""): AxiStreamSlave = {
      Bfm(new AxiStreamSlave(iface, peek, poke, println, ident, rnd))
    }

    def create_axilite_master(iface: AxiLiteIf, ident: String = ""): AxiLiteMaster = {
      Bfm(new AxiLiteMaster(iface, peek, poke, println))
    }
  }
}
