package omnixtend

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class MemRWModule(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(Seq(TLClientPortParameters(Seq(
    TLClientParameters(name = "mem-rw-client")
  ))))

  lazy val module = new LazyModuleImp(this) {
    val (tl_out, edge) = node.out(0)

    val addr     = 0x80000000L.U(64.W)
    val writeVal = 0xCAFEBABEDEADBEEFL.U(64.W)
    val readVal  = Reg(UInt(64.W))

    val sIdle :: sWrite :: sRead :: sDone :: Nil = Enum(4)
    val state = RegInit(sIdle)

    val a_valid = WireDefault(false.B)
    val a_bits = WireDefault(0.U.asTypeOf(tl_out.a.bits))
    tl_out.a.valid := a_valid
    tl_out.a.bits  := a_bits
    tl_out.d.ready := true.B

    switch(state) {
      is(sIdle) {
        val (_, put_bits) = edge.Put(0.U, addr, log2Ceil(8).U, writeVal)
        a_valid := true.B
        a_bits  := put_bits
        when(tl_out.a.fire()) { state := sRead }
      }
      is(sRead) {
        val (_, get_bits) = edge.Get(1.U, addr, log2Ceil(8).U)
        a_valid := true.B
        a_bits  := get_bits
        when(tl_out.a.fire()) { state := sDone }
      }
      is(sDone) {
        when(tl_out.d.valid && tl_out.d.bits.opcode === TLMessages.AccessAckData) {
          readVal := tl_out.d.bits.data
        }
      }
    }

    val io = IO(new Bundle {
      val readResult = Output(UInt(64.W))
    })

    io.readResult := readVal
  }
}