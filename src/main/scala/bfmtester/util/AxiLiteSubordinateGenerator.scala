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

package bfmtester.util

import bfmtester.AxiLiteIf
import chisel3._
import chisel3.experimental.DataMirror
import chisel3.internal.requireIsChiselType
import chisel3.util._

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map}
import scala.util.control.Breaks.{break, breakable}

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
  generate_reg_wr_pulse(area_map, regs)
  when(wr_en) {
    generate_reg_write(area_map, regs, wr_addr, wr_data, wr_strb)
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

  /** Create a Python header from an AreaMap */
  def gen_python_header(area_map: AreaMap, class_filename: String, out_filename: String): Unit = {
    val regs_sorted = area_map.els
      .filter(_.isInstanceOf[Reg])
      .sortBy(_.asInstanceOf[Reg].addr)

    val reg_addr_last = regs_sorted.last.asInstanceOf[AxiLiteSubordinateGenerator.Reg].addr

    var reg_dict: Map[Int, Reg] = Map[Int, Reg]()
    for (el <- area_map.els.filter(_.isInstanceOf[Reg])) {
      val reg = el.asInstanceOf[Reg]
      reg_dict += (reg.addr -> reg)
    }

    val file = new File(out_filename)
    val bw = new BufferedWriter(new FileWriter(file))

    bw.write("# auto-generated with AxiLiteSubordinateGenerator from chisel-bfm-tester\n\n")
    bw.write("import ctypes\n\n\n")
    bw.write(s"class ${class_filename}(ctypes.Structure):\n")
    bw.write("    _pack_ = 1\n")
    bw.write("    _fields_ = [\n")

    for (i <- 0 to reg_addr_last by 4) {
      val s: String = reg_dict.get(i).map(_.name).getOrElse(f"rsvd0x${i}%x")
      bw.write(s"""        ("${s}", ctypes.c_uint32),\n""")
    }

    bw.write("    ]\n")
    bw.close()
  }

  def gen_c_header(
      area_map: AreaMap,
      class_filename: String,
      out_filename: String,
      use_cpp_header: Boolean = false
  ): Unit = {
    val regs_sorted = area_map.els
      .filter(_.isInstanceOf[Reg])
      .sortBy(_.asInstanceOf[Reg].addr)

    val reg_addr_last = regs_sorted.last.asInstanceOf[AxiLiteSubordinateGenerator.Reg].addr

    var reg_dict: mutable.Map[Int, Reg] = mutable.Map[Int, Reg]()
    for (el <- area_map.els.filter(_.isInstanceOf[Reg])) {
      val reg = el.asInstanceOf[Reg]
      reg_dict += (reg.addr -> reg)
    }

    val file = new File(out_filename)
    val bw = new BufferedWriter(new FileWriter(file))

    bw.write("// auto-generated with AxiLiteSubordinateGenerator from chisel-bfm-tester\n\n")

    if (use_cpp_header) {
      bw.write("#include <cstdint>\n\n")
    } else {
      bw.write("#include <stdint.h>\n\n")
    }
    bw.write(s"struct __attribute__((__packed__)) ${class_filename}_regs {\n")

    for (addr <- 0 to reg_addr_last by 4) {
      val reg = reg_dict.get(addr)

      var s: String = ""
      if (reg.isEmpty) {
        s = f"  uint32_t rsvd0x${addr}%x;"
      } else {
        if (reg.get.fields.length == 1 && reg.get.fields.head.name == "") {
          s = s"  uint32_t ${reg.get.name};  // single reg"
        } else {
          s = s"  struct __attribute__((__packed__)) {\n"

          val field_dict: mutable.Map[Int, (String, Int)] = mutable.Map[Int, (String, Int)]()
          for (field <- reg.get.fields) {
            if (field.sw_access != AxiLiteSubordinateGenerator.Access.NA) {
              field_dict += (field.lo
                .getOrElse(field.hi) -> (field.name, field.hi - field.lo.getOrElse(field.hi) + 1))
            }
          }

          breakable {
            var i = 0
            while (true) {
              if (field_dict.contains(i)) {
                val field = field_dict(i)
                s += s"    uint32_t ${field._1} : ${field._2};\n"
                i += field._2
              } else {
                s += s"    uint32_t rsvd${i} : 1;\n"
                i += 1
              }
              if (i >= 32) {
                break
              }
            }
          }
          val name = reg.get.name
          s += s"  } ${name};"
        }
      }
      bw.write(s"${s}\n\n")
    }

    bw.write("};\n")
    bw.close()
  }

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
      wr_data: UInt,
      wr_strb: UInt
  ): Unit = {
    for (el <- area_map.els if el.isInstanceOf[Reg]) {
      val reg = el.asInstanceOf[Reg]
      for (field <- reg.fields) {
        if (field.sw_access == Access.W || field.sw_access == Access.RW) {
          val lo = field.lo.getOrElse(field.hi)
          when(wr_addr === (reg.addr / 4).U) {
            val prev: UInt = Cat(0.U(32.W), reg_fields_to_uint(reg, regs))
            val tmp = Wire(Vec(32 / 8, UInt(8.W)))
            tmp := DontCare

            for (i <- 0 until 4) {
              when((wr_strb & (1 << i).U) =/= 0.U) {
                tmp(i) := wr_data((i + 1) * 8 - 1, i * 8)
              }.otherwise {
                tmp(i) := prev((i + 1) * 8 - 1, i * 8)
              }
            }

            regs(field_name(reg, field)) := (tmp.asUInt() >> lo)(field.hi - lo, 0)
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
