/*
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

package bfmtester.utils

import bfmtester.AxiLiteIf
import chisel3._
import chisel3.experimental.DataMirror
import chisel3.internal.requireIsChiselType
import chisel3.util._

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map}

class AxiLiteSubordinateGenerator(
    area_map: AxiLiteSubordinateGenerator.AreaMap,
    val addr_w: Int = 8
) extends Module {

  import AxiLiteSubordinateGenerator._

  val inp_fields = generate_hw_inputs(area_map)
  val out_fields = generate_hw_outputs(area_map)

  val io = IO(new Bundle {
    val inp = Input(inp_fields)
    val out = Output(out_fields)
    val ctrl = new AxiLiteIf(addr_w = addr_w.W)
  })

  //==========================================================================
  // regs

  val regs: RegType = generate_regs(area_map)

  generate_reg_update(area_map, regs, io.inp)
  generate_reg_output(area_map, regs, io.out)

  //==========================================================================
  // write part

  val sWrIdle :: sWrHasRecvAddr :: sWrHasRecvData :: sWrResp :: Nil = Enum(4)
  val state_wr = RegInit(sWrIdle)

  val wr_en = Reg(Bool())
  val wr_addr = Reg(UInt())
  val wr_data = Reg(UInt())
  val wr_strb = Reg(UInt())

  // default value (gets overridden by FSM when both data and addr are valid)
  // this is just to make the compiler happy
  wr_en := false.B

  switch(state_wr) {
    is(sWrIdle) {
      when(io.ctrl.AW.valid && io.ctrl.W.valid) {
        wr_en := true.B
        wr_addr := io.ctrl.AW.bits.addr(addr_w - 1, 2)
        wr_data := io.ctrl.W.bits.wdata
        wr_strb := io.ctrl.W.bits.wstrb
        state_wr := sWrResp
      }.elsewhen(io.ctrl.AW.valid) {
          wr_addr := io.ctrl.AW.bits.addr(addr_w - 1, 2)
          state_wr := sWrHasRecvAddr
        }
        .elsewhen(io.ctrl.W.valid) {
          wr_data := io.ctrl.W.bits.wdata
          wr_strb := io.ctrl.W.bits.wstrb
          state_wr := sWrHasRecvData
        }
    }
    is(sWrHasRecvAddr) {
      when(io.ctrl.W.valid) {
        wr_en := true.B
        wr_data := io.ctrl.W.bits.wdata
        wr_strb := io.ctrl.W.bits.wstrb
        state_wr := sWrResp
      }
    }
    is(sWrHasRecvData) {
      when(io.ctrl.AW.valid) {
        wr_en := true.B
        wr_addr := io.ctrl.AW.bits.addr(addr_w - 1, 2)
        state_wr := sWrResp
      }
    }
    is(sWrResp) {
      when(io.ctrl.B.ready) {
        state_wr := sWrIdle
      }
    }
  }

  // default values (gets overridden by FSM when both addr is valid)
  // this is just to make the compiler happy
  io.ctrl.AW.ready := false.B
  io.ctrl.W.ready := false.B
  io.ctrl.B.valid := false.B

  switch(state_wr) {
    is(sWrIdle) {
      io.ctrl.AW.ready := true.B
      io.ctrl.W.ready := true.B
      io.ctrl.B.valid := false.B
    }
    is(sWrHasRecvData) {
      io.ctrl.AW.ready := true.B
      io.ctrl.W.ready := false.B
      io.ctrl.B.valid := false.B
    }
    is(sWrHasRecvAddr) {
      io.ctrl.AW.ready := false.B
      io.ctrl.W.ready := true.B
      io.ctrl.B.valid := false.B
    }
    is(sWrResp) {
      io.ctrl.AW.ready := false.B
      io.ctrl.W.ready := false.B
      io.ctrl.B.valid := true.B
    }
  }

  // as in the Xilinx example, we always return OKAY as a response
  io.ctrl.B.bits := 0x0.U(2.W)
  io.ctrl.R.bits.rresp := 0x0.U(2.W)

  // write to regs
  def wrWithStrobe(data: UInt, prev: UInt, strobe: UInt): UInt = {
    val BIT_W = 8
    val tmp = Wire(Vec(prev.getWidth / BIT_W, UInt(BIT_W.W)))

    for (i <- 0 until prev.getWidth / BIT_W) {
      when((strobe & (1 << i).U) =/= 0.U) {
        tmp(i) := data((i + 1) * BIT_W - 1, i * BIT_W)
      }.otherwise {
        tmp(i) := prev((i + 1) * BIT_W - 1, i * BIT_W)
      }
    }

    tmp.asUInt()
  }

  generate_reg_wr_pulse(area_map, regs)
  when(wr_en) {
    generate_reg_write(area_map, regs, wr_addr, wr_data)
  }

  //==========================================================================
  // read part

  val sRdIdle :: sMemCyc :: sRdRead :: sRdResp :: Nil = Enum(4)
  val state_rd = RegInit(sRdIdle)

  val rd_en = Reg(Bool())
  val rd_addr = Reg(UInt())
  val rd_data = Reg(UInt(io.ctrl.R.bits.getWidth.W))

  // default value (gets overridden by FSM when both data and addr are valid)
  rd_en := false.B

  switch(state_rd) {
    is(sRdIdle) {
      when(io.ctrl.AR.valid) {
        rd_en := true.B
        rd_addr := io.ctrl.AR.bits.addr(addr_w - 1, 2)
        when(addr_is_mem(io.ctrl.AR.bits.addr(addr_w - 1, 2), area_map).asBool()) {
          state_rd := sMemCyc
        }.otherwise {
          state_rd := sRdRead
        }
      }
    }
    is(sMemCyc) {
      state_rd := sRdRead
    }
    is(sRdRead) {
      state_rd := sRdResp
    }
    is(sRdResp) {
      when(io.ctrl.R.ready) {
        state_rd := sWrIdle
      }
    }
  }

  io.ctrl.AR.ready := false.B
  io.ctrl.R.valid := false.B
  io.ctrl.R.bits.rdata := 0.U

  switch(state_rd) {
    is(sRdIdle) {
      io.ctrl.AR.ready := true.B
      io.ctrl.R.valid := false.B
      io.ctrl.R.bits.rdata := 0.U
    }
    is(sMemCyc) {
      io.ctrl.AR.ready := false.B
      io.ctrl.R.valid := false.B
      io.ctrl.R.bits.rdata := 0.U
    }
    is(sRdRead) {
      io.ctrl.AR.ready := false.B
      io.ctrl.R.valid := false.B
      io.ctrl.R.bits.rdata := 0.U
    }
    is(sRdResp) {
      io.ctrl.AR.ready := false.B
      io.ctrl.R.valid := true.B
      io.ctrl.R.bits.rdata := rd_data
    }
  }

  // read from regs
  when(rd_en && !addr_is_mem(rd_addr, area_map).asBool()) {
    generate_reg_read(area_map, regs, rd_addr, rd_data)
  }

  when(state_rd === sRdIdle) {
    generate_mem_read(area_map, regs, io.ctrl.AR.bits.addr(addr_w - 1, 2))
  }

  update_mem_read(area_map, regs, rd_data)
}

object AxiLiteSubordinateGenerator {
  // inspired by https://github.com/chipsalliance/chisel3/blob/master/src/test/scala/chiselTests/RecordSpec.scala
  final class Axi4LiteSubordinateGenericBundle(elts: (String, Data)*) extends Record {
    val elements = ListMap(elts map {
      case (field, elt) =>
        requireIsChiselType(elt)
        field -> elt
    }: _*)
    def apply(elt: String): Data = elements(elt)
    override def cloneType: this.type = {
      val cloned = elts.map { case (n, d) => n -> DataMirror.internal.chiselTypeClone(d) }
      (new Axi4LiteSubordinateGenericBundle(cloned: _*)).asInstanceOf[this.type]
    }
  }

  object Access extends Enumeration {
    type Access = Value
    val R, W, RW, NA = Value
  }

  class Field(
      val name: String,
      val hw_access: Access.Access,
      val sw_access: Access.Access,
      val hi: Int,
      val lo: Option[Int] = None,
      val reset: Option[UInt] = None,
      val singlepulse: Boolean = false
  ) {}

  class Els(val name: String) {}

  class Reg(override val name: String, val addr: Int, val fields: Field*) extends Els(name) {}

  class Mem(override val name: String, val addr: Int, val data_w: Int, val nr_els: Int)
      extends Els(name) {}

  class AreaMap(val els: Els*) {}

  private def field_name(reg: Reg, field: Field): String = {
    reg.name + "_" + field.name
  }

  private def field_type(reg: Reg, field: Field): Data = {
    if (field.lo.isDefined) { UInt((field.hi - field.lo.get + 1).W) }
    else { Bool() }
  }

  private def generate_hw_inputs_or_outputs(
      area_map: AreaMap,
      is_input: Boolean
  ): Axi4LiteSubordinateGenericBundle = {

    val ports: ListBuffer[(String, Data)] = ListBuffer[(String, Data)]()

    val field_filt: Field => Boolean = if (is_input) { (field: Field) =>
      field.hw_access == Access.W || field.hw_access == Access.RW
    } else { (field: Field) =>
      field.hw_access == Access.R || field.hw_access == Access.RW
    }

    // regs
    for (el <- area_map.els if el.isInstanceOf[Reg]) {
      val reg = el.asInstanceOf[Reg]
      for (field <- reg.fields) {
        if (field_filt(field)) {
          val f_typ = field_type(reg, field)
          ports += field_name(reg, field) -> f_typ
        }
      }
    }

    // mems
    for (el <- area_map.els if el.isInstanceOf[Mem]) {
      val mem = el.asInstanceOf[Mem]
      if (is_input) {
        ports += "MEM_" + mem.name + "_DOUT" -> UInt(mem.data_w.W)
      } else {
        ports += "MEM_" + mem.name + "_DIN" -> UInt(mem.data_w.W)
        ports += "MEM_" + mem.name + "_ADDR" -> UInt(log2Ceil(mem.nr_els).W)
        ports += "MEM_" + mem.name + "_WE" -> Bool()
      }
    }

    new Axi4LiteSubordinateGenericBundle(ports: _*)
  }

  private def generate_hw_inputs(area_map: AreaMap): Axi4LiteSubordinateGenericBundle = {
    generate_hw_inputs_or_outputs(area_map, is_input = true)
  }

  private def generate_hw_outputs(area_map: AreaMap): Axi4LiteSubordinateGenericBundle = {
    generate_hw_inputs_or_outputs(area_map, is_input = false)
  }

  type RegType = mutable.Map[String, Data]
  private def generate_regs(area_map: AreaMap): RegType = {
    val m: RegType = mutable.Map()

    for (el <- area_map.els if el.isInstanceOf[Reg]) {
      val reg = el.asInstanceOf[Reg]
      for (field <- reg.fields) {
        val f_typ = field_type(reg, field)
        val r = if (field.reset.isDefined) {
          RegInit(f_typ, field.reset.get)
        } else {
          Reg(f_typ)
        }
        m += field_name(reg, field) -> r.suggestName(field_name(reg, field))
      }
    }

    for (el <- area_map.els if el.isInstanceOf[Mem]) {
      val mem = el.asInstanceOf[Mem]
      m += "MEM_" + mem.name + "_DIN" -> Reg(UInt(mem.data_w.W))
        .suggestName("MEM_" + mem.name + "_DIN")
      m += "MEM_" + mem.name + "_DOUT" -> Wire(UInt(mem.data_w.W))
        .suggestName("MEM_" + mem.name + "_DOUT")
      m += "MEM_" + mem.name + "_ADDR" -> Reg(UInt(log2Ceil(mem.nr_els).W))
        .suggestName("MEM_" + mem.name + "_ADDR")
      m += "MEM_" + mem.name + "_WE" -> RegInit(Bool(), false.B)
        .suggestName("MEM_" + mem.name + "_WE")

      val reg_act1 = RegInit(Bool(), false.B).suggestName("MEM_" + mem.name + "_ACT1")
      m += "MEM_" + mem.name + "_ACT1" -> reg_act1
      m += "MEM_" + mem.name + "_ACT2" -> RegNext(reg_act1)
        .suggestName("MEM_" + mem.name + "_ACT2")
    }

    m
  }

  private def generate_reg_update(area_map: AreaMap, regs: RegType, record: Record): Unit = {
    for (el <- area_map.els if el.isInstanceOf[Reg]) {
      val reg = el.asInstanceOf[Reg]
      for (field <- reg.fields) {
        if (field.hw_access == Access.RW || field.hw_access == Access.W) {
          regs(field_name(reg, field)) := record.elements(field_name(reg, field))
        }
      }
    }

    for (el <- area_map.els if el.isInstanceOf[Mem]) {
      val mem = el.asInstanceOf[Mem]
      regs("MEM_" + mem.name + "_DOUT") := record.elements("MEM_" + mem.name + "_DOUT")
    }
  }

  private def generate_reg_output(area_map: AreaMap, regs: RegType, record: Record): Unit = {
    for (el <- area_map.els if el.isInstanceOf[Reg]) {
      val reg = el.asInstanceOf[Reg]
      for (field <- reg.fields) {
        if (field.hw_access == Access.RW || field.hw_access == Access.R) {
          record.elements(field_name(reg, field)) := regs(field_name(reg, field))
        }
      }
    }

    for (el <- area_map.els if el.isInstanceOf[Mem]) {
      val mem = el.asInstanceOf[Mem]
      record.elements("MEM_" + mem.name + "_DIN") := regs("MEM_" + mem.name + "_DIN")
      record.elements("MEM_" + mem.name + "_ADDR") := regs("MEM_" + mem.name + "_ADDR")
      record.elements("MEM_" + mem.name + "_WE") := regs("MEM_" + mem.name + "_WE")
    }
  }

  private def reg_fields_to_uint(reg: Reg, regs: RegType): UInt = {
    val bools: Vec[Bool] = VecInit.tabulate(32)(_ => false.B)

    for (field <- reg.fields) {
      if (field.sw_access == Access.R || field.sw_access == Access.RW) {
        if (field.lo.isDefined) {
          for (i <- field.lo.get to field.hi) {
            bools(i) := regs(field_name(reg, field)).asUInt()(i - field.lo.get)
          }
        } else {
          bools(field.hi) := regs(field_name(reg, field)).asUInt()
        }
      }
    }
    bools.asUInt()
  }

  private def generate_reg_read(
      area_map: AreaMap,
      regs: RegType,
      rd_addr: UInt,
      rd_data: UInt
  ): Unit = {

    rd_data := 0xdeadbeefL.U

    for (el <- area_map.els if el.isInstanceOf[Reg]) {
      val reg = el.asInstanceOf[Reg]
      when(rd_addr === (reg.addr / 4).U) {
        rd_data := reg_fields_to_uint(reg, regs)
      }
    }
  }

  private def generate_mem_read(
      area_map: AreaMap,
      regs: RegType,
      rd_addr: UInt
  ): Unit = {
    for (el <- area_map.els if el.isInstanceOf[Mem]) {
      val mem = el.asInstanceOf[Mem]
      val mem_lo = mem.addr / 4
      val mem_hi = (mem.addr + mem.nr_els * 4) / 4

      when(rd_addr >= mem_lo.U && rd_addr < mem_hi.U) {
        regs("MEM_" + mem.name + "_ADDR") := rd_addr - mem_lo.U
        regs("MEM_" + mem.name + "_ACT1") := true.B
      }
    }
  }

  private def generate_reg_wr_pulse(
      area_map: AreaMap,
      regs: RegType
  ): Unit = {
    for (el <- area_map.els if el.isInstanceOf[Reg]) {
      val reg = el.asInstanceOf[Reg]
      for (field <- reg.fields) {
        if (field.sw_access == Access.RW && field.singlepulse) {
          regs(field_name(reg, field)) := 0.U
        }
      }
    }

    for (el <- area_map.els if el.isInstanceOf[Mem]) {
      val mem = el.asInstanceOf[Mem]
      regs("MEM_" + mem.name + "_ACT1") := false.B
      regs("MEM_" + mem.name + "_WE") := false.B
    }
  }

  private def generate_reg_write(
      area_map: AreaMap,
      regs: RegType,
      wr_addr: UInt,
      wr_data: UInt
  ): Unit = {
    for (el <- area_map.els if el.isInstanceOf[Reg]) {
      val reg = el.asInstanceOf[Reg]
      for (field <- reg.fields) {
        if (field.sw_access == Access.W || field.sw_access == Access.RW) {
          val lo = field.lo.getOrElse(field.hi)
          when(wr_addr === (reg.addr / 4).U) {
            regs(field_name(reg, field)) := wr_data(field.hi, lo)
          }
        }
      }
    }

    for (el <- area_map.els if el.isInstanceOf[Mem]) {
      val mem = el.asInstanceOf[Mem]
      val mem_lo = mem.addr / 4
      val mem_hi = (mem.addr + mem.nr_els * 4) / 4

      when(wr_addr >= mem_lo.U && wr_addr < mem_hi.U) {
        regs("MEM_" + mem.name + "_DIN") := wr_data
        regs("MEM_" + mem.name + "_ADDR") := wr_addr - mem_lo.U
        regs("MEM_" + mem.name + "_WE") := true.B
      }
    }
  }

  def update_mem_read(area_map: AreaMap, regs: RegType, rd_data: UInt): Unit = {
    for (el <- area_map.els if el.isInstanceOf[Mem]) {
      val mem = el.asInstanceOf[Mem]

      when(regs("MEM_" + mem.name + "_ACT2").asUInt().asBool()) {
        rd_data := regs("MEM_" + mem.name + "_DOUT")
      }
    }
  }

  def addr_is_mem(addr: UInt, area_map: AreaMap): Bool = {
    val is_mem = WireInit(false.B)

    for (el <- area_map.els if el.isInstanceOf[Mem]) {
      val mem = el.asInstanceOf[Mem]

      val mem_lo = mem.addr / 4
      val mem_hi = (mem.addr + mem.nr_els * 4) / 4

      when(addr >= mem_lo.U && addr < mem_hi.U) {
        is_mem := true.B
      }
    }
    is_mem
  }
}
