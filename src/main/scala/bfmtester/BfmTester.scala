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

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import chisel3._
import chisel3.iotesters._

import scala.collection.mutable

/** Chisel tester with Bus Functional Models */
abstract class BfmTester[+T <: MultiIOModule](dut: T) extends PeekPokeTester(dut) {
  private val rm = runtimeMirror(getClass.getClassLoader)
  private val im = rm.reflect(this)
  private val members = im.symbol.typeSignature.members
  private def bfms: Iterable[universe.Symbol] = members.filter(_.typeSignature <:< typeOf[Bfm])

  private val update_queue = new mutable.Queue[(Bits, BigInt)]

  private def poke_onto_queue(signal: Bits, value: BigInt): Unit = {
    update_queue.enqueue((signal, value))
  }

  private def stepSingle(): Unit = {
    for (bfm <- bfms) {
      im.reflectField(bfm.asTerm).get.asInstanceOf[Bfm].update(t, poke_onto_queue)
    }
    for (el <- update_queue) {
      poke(el._1, el._2)
    }
    update_queue.clear()
    super.step(1)
  }

  override def step(n: Int): Unit = {
    for (_ <- 0 until n) {
      stepSingle()
    }
  }

  def bfm_peek(bs: Bits): BigInt = this.peek(bs)
  def bfm_poke(bs: Bits, x: BigInt): Unit = this.poke(bs, x)

  object BfmFactory {
    def create_avalon_slave(iface: AvalonMMIf, ident: String = ""): AvalonMMMemSlave = {
      new AvalonMMMemSlave(iface, rnd, bfm_peek, bfm_poke, println, ident)
    }

    def create_axis_master(iface: AxiStreamIf, ident: String = ""): AxiStreamMaster = {
      new AxiStreamMaster(iface, bfm_peek, bfm_poke, println, ident)
    }

    def create_axis_slave(iface: AxiStreamIf, ident: String = ""): AxiStreamSlave = {
      new AxiStreamSlave(iface, bfm_peek, bfm_poke, println, ident, rnd)
    }

    def create_axilite_master(iface: AxiLiteIf, ident: String = ""): AxiLiteMaster = {
      new AxiLiteMaster(iface, bfm_peek, bfm_poke, println)
    }

    def create_axi_slave(iface: AxiIf, ident: String = ""): AxiMemSlave = {
      new AxiMemSlave(iface, rnd, bfm_peek, bfm_poke, println, ident)
    }

    def create_pulse_detector(iface: Bool, ident: String = ""): PulseDetector = {
      new PulseDetector(iface, bfm_peek, bfm_poke, println, ident)
    }

  }
}
