/*
MIT License

Copyright (c) 2018-2021 Jan Marjanovic

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

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

class AvalonMMMemSlave(
    val avalon: AvalonMMIf,
    val rnd: Random,
    val peek: Bits => BigInt,
    val poke: (Bits, BigInt) => Unit,
    val println: String => Unit,
    val ident: String = ""
) extends Bfm {

  private var verbose: Boolean = true
  private val sparse_mem: mutable.HashMap[BigInt, Byte] = new mutable.HashMap()
  private val width_bits: Int = avalon.writedata.getWidth

  def setVerbose(enable_verbose: Boolean) {
    verbose = enable_verbose
  }

  private def printWithBg(s: String): Unit = {
    // black on magenta
    println("\u001b[97;44m" + s + "\u001b[39;49m")
  }

  def mem_set(start_addr: BigInt, els: Seq[Byte]): Unit = {
    for ((el, i) <- els.zipWithIndex) {
      sparse_mem += Tuple2(start_addr + i, el)
    }
  }

  def mem_get_el(addr: BigInt): Byte = {
    // TODO: add configurable handling for when the mem is empty
    sparse_mem(addr)
  }

  def mem_get_word(addr: BigInt, word_size: Int): BigInt = {
    var tmp: BigInt = 0
    for (i <- word_size - 1 to 0 by -1) {
      tmp <<= 8
      tmp |= mem_get_el(addr + i) & 0xff
    }
    tmp
  }

  def mem_set_word(start_addr: BigInt, data: BigInt, word_size: Int): Unit = {
    for (i <- 0 until word_size) {
      val b = (data >> (8 * i)) & 0xff
      mem_set(start_addr + i, List[Byte](b.toByte))
    }
  }

  def mem_stats(): Unit = {
    printWithBg(f"      AvalonMMMemSlave($ident): Mem nr_els = ${sparse_mem.size}")
  }

  private var addr: Option[BigInt] = None
  private var burst_rem: BigInt = 0

  def update_wr(waitrequest: Boolean, t: Long, poke: (Bits, BigInt) => Unit): Unit = {
    //val waitrequest = peek(avalon.waitrequest) > 0
    val write = peek(avalon.write) > 0
    if (!waitrequest && write) {
      if (addr.isEmpty) {
        addr = Some(peek(avalon.address))
        burst_rem = peek(avalon.burstcount) - 1
        printWithBg(
          f"${t}%5d AvalonMMMemSlave($ident): Recv wr addr (addr=0x${addr.get}%x, burst_rem=${burst_rem})"
        )
      }

      val data = peek(avalon.writedata)
      // TODO: byte enable

      printWithBg(
        f"${t}%5d AvalonMMMemSlave($ident): Write to addr (addr=0x${addr.get}%x, data=0x${data}%032x, burst_rem=${burst_rem})"
      )
      mem_set_word(addr.get, data, width_bits / 8)

      if (burst_rem == 0) {
        addr = None
      } else {
        addr = addr.map(_ + BigInt(width_bits / 8))
        burst_rem -= 1
      }

    }
  }

  private var read_addrs: ListBuffer[BigInt] = ListBuffer()

  def update_rd(waitrequest: Boolean, t: Long, poke: (Bits, BigInt) => Unit): Unit = {
    // we first handle the readdata and readdatavalid to produce at least 1 clk cycle delay between addr and data

    val readdatavalid = rnd.nextBoolean()
    if (read_addrs.nonEmpty && readdatavalid) {
      val rd_addr = read_addrs.remove(0)

      // TODO: configurable
      val data: BigInt =
        try {
          mem_get_word(rd_addr, width_bits / 8)
        } catch {
          case _: java.util.NoSuchElementException => 0xdeadbeef
        }

      poke(avalon.readdatavalid, 1)
      poke(avalon.readdata, data)
      printWithBg(
        f"${t}%5d AvalonMMMemSlave($ident): Read from addr (addr=0x${rd_addr}%x, data=0x${data}%032x)"
      )
    } else {
      poke(avalon.readdatavalid, 0)
      poke(avalon.readdata, 0)
    }

    val read = peek(avalon.read) > 0
    if (!waitrequest && read) {
      val address = peek(avalon.address)
      val burstcount = peek(avalon.burstcount).toInt
      printWithBg(
        f"${t}%5d AvalonMMMemSlave($ident): Recv rd addr (addr=0x${address}%x, burst_rem=${burstcount})"
      )
      for (i <- 0 until burstcount) {
        read_addrs += address + i * width_bits / 8
      }
    }
  }

  override def update(t: Long, poke: (Bits, BigInt) => Unit): Unit = {
    val waitrequest: Boolean = rnd.nextBoolean()
    poke(avalon.waitrequest, if (waitrequest) 1 else 0)
    update_wr(waitrequest, t, poke)
    update_rd(waitrequest, t, poke)
  }

  printWithBg(f"      AvalonMMMemSlave($ident): BFM initialized")
  printWithBg(f"      AvalonMMMemSlave($ident): BETA VERSION - use with caution!")

}
