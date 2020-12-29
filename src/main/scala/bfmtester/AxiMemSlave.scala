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

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

/** Memory slave for AXI4 interface
  *
  * Contains an internal memory, which can be read and written to using the AXI
  * interface. The BFM also provides methods to manipulate the internal memory
  * from the testbench.
  *
  */
class AxiMemSlave(val axi: AxiIf,
                  val rnd: Random,
                  val peek: Bits => BigInt,
                  val poke: (Bits, BigInt) => Unit,
                  val println: String => Unit,
                  val ident: String = "")
    extends Bfm {

  private var verbose: Boolean = true
  private val sparse_mem: mutable.HashMap[BigInt, Byte] = new mutable.HashMap()
  private val width_bits: Int = axi.W.bits.data.getWidth

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
      val b = (data >> (8 * i)) & 0xFF
      mem_set(start_addr + i, List[Byte](b.toByte))
    }
  }

  def mem_stats(): Unit = {
    printWithBg(f"      AxiMemSlave($ident): Mem nr_els = ${sparse_mem.size}")
  }

  private class Addr(val addr: BigInt, val len: BigInt, val id: BigInt) {}

  private class ReadCh {
    private val read_addrs: ListBuffer[Addr] = ListBuffer[Addr]()
    private var act_read_addr: Option[Addr] = None

    def update_read(t: Long, poke: (Bits, BigInt) => Unit): Unit = {
      // AR
      val arready: Boolean = rnd.nextBoolean()
      val arready_prev: Boolean = peek(axi.AR.ready) > 0
      val arvalid: Boolean = peek(axi.AR.valid) > 0
      poke(axi.AR.ready, if (arready) 1 else 0)
      if (arready_prev & arvalid) {
        val addr = peek(axi.AR.bits.addr)
        val len = peek(axi.AR.bits.len)
        val id = peek(axi.AR.bits.id)
        read_addrs += new Addr(addr, len, id)
        printWithBg(
          f"${t}%5d AxiMemSlave($ident): Read Address (addr=0x${addr}%x, len=${len}, id=0x${id}%x)"
        )
      }

      // R
      if (read_addrs.nonEmpty && act_read_addr.isEmpty) {
        act_read_addr = Option(read_addrs.remove(0))
      }

      // TODO: handle RREADY
      if (act_read_addr.nonEmpty) {
        val addr: Addr = act_read_addr.get
        val data: BigInt = mem_get_word(addr.addr, width_bits / 8)
        printWithBg(
          f"${t}%5d AxiMemSlave($ident): Read Data (addr=0x${addr.addr}%x, data=0x${data}%032x, rem len=${addr.len})"
        )
        poke(axi.R.bits.data, data)
        poke(axi.R.valid, 1)
        poke(axi.R.bits.resp, 0)
        poke(axi.R.bits.last, if (addr.len == 0) 1 else 0)
        poke(axi.R.bits.id, addr.id)
        if (addr.len == 0) {
          act_read_addr = None
        } else {
          val new_addr =
            new Addr(addr.addr + width_bits / 8, addr.len - 1, addr.id)
          act_read_addr = Option(new_addr)
        }
      } else {
        poke(axi.R.bits.data, 0)
        poke(axi.R.valid, 0)
        poke(axi.R.bits.resp, 0)
        poke(axi.R.bits.last, 0)
        poke(axi.R.bits.id, 0)
      }
    }
  }

  private class WriteCh {
    private val write_addrs: ListBuffer[Addr] = ListBuffer[Addr]()
    private var act_write_addr: Option[Addr] = None
    private var gen_resp: Boolean = false

    def update_write_addr(t: Long, poke: (Bits, BigInt) => Unit): Unit = {
      // AW
      val awready: Boolean = rnd.nextBoolean()
      val awready_prev: Boolean = peek(axi.AW.ready) > 0
      val awvalid: Boolean = peek(axi.AW.valid) > 0
      poke(axi.AW.ready, if (awready) 1 else 0)
      if (awready_prev & awvalid) {
        val addr = peek(axi.AW.bits.addr)
        val len = peek(axi.AW.bits.len)
        val id = peek(axi.AW.bits.id)
        write_addrs += new Addr(addr, len, id)
        printWithBg(
          f"${t}%5d AxiMemSlave($ident): Write Address (addr=0x${addr}%x, len=${len}, id=0x${id}%x)"
        )
      }
    }

    def update_write_data(t: Long, poke: (Bits, BigInt) => Unit): Unit = {
      // W
      val wready: Boolean = rnd.nextBoolean()
      val wready_prev: Boolean = peek(axi.W.ready) > 0
      val wvalid: Boolean = peek(axi.W.valid) > 0
      poke(axi.W.ready, if (wready) 1 else 0)
      if (wready_prev & wvalid) {
        // if we do not have an address yet, take the first one from the list
        if (act_write_addr.isEmpty && write_addrs.nonEmpty) {
          act_write_addr = Some(write_addrs.remove((0)))
        }

        // store data in the internal memory
        if (act_write_addr.nonEmpty) {
          val addr: Addr = act_write_addr.get
          val data = peek(axi.W.bits.data)
          val last = peek(axi.W.bits.last)

          printWithBg(
            f"${t}%5d AxiMemSlave($ident): Write Data (addr=0x${addr.addr}%x, data=0x${data}%032x, len=${addr.len}, last=${last})"
          )

          mem_set_word(addr.addr, data, width_bits / 8)

          if (addr.len == 0) {
            act_write_addr = None
            gen_resp = true
          } else {
            val new_addr =
              new Addr(addr.addr + width_bits / 8, addr.len - 1, addr.id)
            act_write_addr = Option(new_addr)
          }
        } else {
          //val addr: Addr = act_write_addr.get
          val data = peek(axi.W.bits.data)

          printWithBg(
            f"${t}%5d AxiMemSlave($ident): Write Data (addr=<not recv>, data=0x${data}%032x, rem len=<not recv>)"
          )
        }
      }
    }
    def update_write_resp(t: Long, poke: (Bits, BigInt) => Unit): Unit = {
      if (gen_resp) {
        poke(axi.B.valid, 1)
        poke(axi.B.bits.resp, AxiIfRespToInt(AxiIfResp.OKAY))
      } else {
        poke(axi.B.valid, 0)
        poke(axi.B.bits.resp, 0)
      }

      val ready = peek(axi.B.ready) > 0
      if (ready) {
        gen_resp = false
      }
    }
  }

  private val rd_ch = new ReadCh()
  private val wr_ch = new WriteCh()

  override def update(t: Long, poke: (Bits, BigInt) => Unit): Unit = {
    rd_ch.update_read(t, poke)
    wr_ch.update_write_addr(t, poke)
    wr_ch.update_write_data(t, poke)
    wr_ch.update_write_resp(t, poke)
  }

  printWithBg(f"      AxiMemSlave($ident): BFM initialized")
}
