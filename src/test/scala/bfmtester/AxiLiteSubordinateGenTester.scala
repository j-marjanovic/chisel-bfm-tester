package bfmtester

class AxiLiteSubordinateGenTester(c: AxiLiteSubordinateExample) extends BfmTester(c) {

  //==========================================================================
  // BFMs
  val mod_axi_mngr = BfmFactory.create_axilite_master(c.io.ctrl)

  //==========================================================================
  // helper functions

  def read_blocking(addr: BigInt, CYC_TIMEOUT: Int = 100): BigInt = {
    mod_axi_mngr.readPush(addr)
    for (_ <- 0 to CYC_TIMEOUT) {
      val resp = mod_axi_mngr.getResponse()
      if (resp.isDefined) {
        return resp.get.rd_data
      }
      step(1)
    }

    throw new RuntimeException("AXI read timeout")
  }

  def write_blocking(addr: BigInt, data: BigInt, strobe: BigInt, CYC_TIMEOUT: Int = 100): Unit = {
    mod_axi_mngr.writePush(addr, data, strobe)
    for (_ <- 0 to CYC_TIMEOUT) {
      val resp = mod_axi_mngr.getResponse()
      if (resp.isDefined) {
        return
      }
      step(1)
    }

    throw new RuntimeException("AXI write timeout")
  }

  //==========================================================================
  // main procedure
  step(10)

  // some basic checks (ID register, version, scratch)
  expect(read_blocking(0) == 0xe8a321e1L, "ID reg")

  write_blocking(0x8, 0xbbaa, 0xf)
  write_blocking(0x8, 0xcc, 0x1)
  expect(read_blocking(0x8) == 0xbbcc, "two writes, different strobes")

  write_blocking(0xc, 0x11223344, 0xf)
  write_blocking(0xc, 0xaa00bb, 0x5)
  expect(read_blocking(0xc) == 0x11aa33bb, "two writes, different strobes")

}
