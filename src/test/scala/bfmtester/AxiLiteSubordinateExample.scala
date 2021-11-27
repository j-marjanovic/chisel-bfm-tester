package bfmtester

import bfmtester.util.AxiLiteSubordinateGenerator
import chisel3._
import bfmtester.util.AxiLiteSubordinateGenerator._

class AxiLiteSubordinateExample extends Module {

  // format: off
  val area_map = new AreaMap(
    new Reg("ID_REG", 0,
      new Field("ID", hw_access = Access.NA, sw_access = Access.R,  hi = 31, Some(0), reset = Some(0xe8a321e1L.U)) // EXAMPLE1
    ),
    new Reg("VERSION", 4,
      new Field("PATCH", hw_access = Access.W, sw_access = Access.R, hi = 7, lo = Some(0), reset = Some(3.U)),
      new Field("MINOR", hw_access = Access.W, sw_access = Access.R, hi = 15, lo = Some(8), reset = Some(2.U)),
      new Field("MAJOR", hw_access = Access.W, sw_access = Access.R, hi = 23, lo = Some(16), reset = Some(1.U))
    ),
    new Reg("SCRATCH", 0x8,
      new Field("FIELD", hw_access = Access.NA, sw_access = Access.RW, hi = 31, lo = Some(0))
    ),
    new Reg("SCRATCH2", 0xc,
      new Field("FIELD0", hw_access = Access.NA, sw_access = Access.RW, hi = 15, lo = Some(0)),
      new Field("FIELD1", hw_access = Access.NA, sw_access = Access.RW, hi = 31, lo = Some(16))
    ),
    new Reg("STATUS", 0x10,
      new Field("DONE_WR", hw_access = Access.W, sw_access = Access.R, hi = 0, lo = None),
      new Field("DONE_RD", hw_access = Access.W, sw_access = Access.R, hi = 1, lo = None),
      new Field("READY_WR", hw_access = Access.W, sw_access = Access.R, hi = 8, lo = None),
      new Field("READY_RD", hw_access = Access.W, sw_access = Access.R, hi = 9, lo = None),
    ),
    new Reg("CONTROL", 0x14,
      new Field("START_WR", hw_access = Access.R, sw_access = Access.RW, hi = 0, lo = None, singlepulse = true),
      new Field("START_RD", hw_access = Access.R, sw_access = Access.RW, hi = 1, lo = None, singlepulse = true),
      new Field("DONE_CLEAR", hw_access = Access.R, sw_access = Access.RW, hi = 8, lo = None, singlepulse = true),
    ),
  )

  val io = IO(new Bundle {
    val ctrl = new AxiLiteIf(8.W, 32.W)
  })

  private val mod_ctrl = Module(
    new AxiLiteSubordinateGenerator(area_map = area_map, addr_w = 8)
  )

  io.ctrl <> mod_ctrl.io.ctrl

  mod_ctrl.io.inp("VERSION_MAJOR") := 0.U
  mod_ctrl.io.inp("VERSION_MINOR") := 2.U
  mod_ctrl.io.inp("VERSION_PATCH") := 1.U
  mod_ctrl.io.inp("STATUS_READY_RD") := false.B
  mod_ctrl.io.inp("STATUS_READY_WR") := false.B
  mod_ctrl.io.inp("STATUS_DONE_RD") := false.B
  mod_ctrl.io.inp("STATUS_DONE_WR") := false.B

}
