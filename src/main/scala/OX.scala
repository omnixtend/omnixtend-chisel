package omnixtend

import chisel3._
import chisel3.util._

import chisel3.experimental.{IntParam, BaseModule}
import freechips.rocketchip.amba.axi4._
// import freechips.rocketchip.subsystem.BaseSubsystem 
import org.chipsalliance.cde.config.{Parameters, Field, Config}
// import freechips.rocketchip.config.{Parameters, Field, Config} 
import freechips.rocketchip.diplomacy._ 
import freechips.rocketchip.subsystem.{BaseSubsystem, MBUS}
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}

case class OXParams(
  address: BigInt = 0x1000,
  width: Int = 32,
  useAXI4: Boolean = false,
  useBlackBox: Boolean = true
)

case object OXKey extends Field[Option[OXParams]](None)


// Definition of OmniXtend Bundle
class OmniXtendBundle extends Bundle {
  // val addr        = Input(UInt(64.W))
  // val valid       = Input(Bool())   // signals if the transaction is valid
  val ready       = Output(Bool())  // signals if the transaction can proceed
  // val in          = Input(UInt(8.W))

  // Connected to Ethernet IP
  val txdata      = Output(UInt(512.W))
  val txvalid     = Output(Bool())
  val txlast      = Output(Bool())
  val txkeep      = Output(UInt(8.W))
  val txready     = Input(Bool())
  val rxdata      = Input(UInt(512.W))
  val rxvalid     = Input(Bool())
  val rxlast      = Input(Bool())

  val ox_open     = Input(Bool())
  val ox_close    = Input(Bool())
  val debug1      = Input(Bool())
  val debug2      = Input(Bool())
}

/**
 * OmniXtendNode is a LazyModule that defines a TileLink manager node
 * which supports OmniXtend protocol operations. It handles Get and PutFullData
 * requests by interfacing with a Transceiver module.
 */
// class OmniXtendNode(implicit p: Parameters) extends ClockSinkDomain(ClockSinkParameters())(p) {
 class OmniXtendNode(implicit p: Parameters) extends LazyModule {
  val beatBytes = 64 // The size of each data beat in bytes
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address            = Seq(AddressSet(0x100000000L, 0x01FFFFFFL)), // Address range this node responds to
    resources          = new SimpleDevice("mem", Seq("example,mem")).reg, // Device resources
    regionType         = RegionType.UNCACHED, // Memory region type
    executable         = true, // Memory is executable
    supportsGet        = TransferSizes(1, beatBytes), // Supported transfer sizes for Get operations
    supportsPutFull    = TransferSizes(1, beatBytes), // Supported transfer sizes for PutFull operations
    supportsPutPartial = TransferSizes(1, beatBytes), // Supported transfer sizes for PutPartial operations
    fifoId             = Some(0) // FIFO ID
  )),
    beatBytes          = 64, // Beat size for the port
    minLatency         = 1 // Minimum latency for the port
  )))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new OmniXtendBundle) // Input/Output bundle

    val (in, edge) = node.in(0) // Getting the input node and its edge

      // Registers for storing the validity of Get and PutFullData operations
      val getValidReg = RegNext(in.a.valid && in.a.bits.opcode === TLMessages.Get, init = false.B)
      val putValidReg = RegNext(in.a.valid && in.a.bits.opcode === TLMessages.PutFullData, init = false.B)
      val aValidReg   = RegInit(false.B) // Register to store the validity of any operation
      val opcodeReg   = RegInit(0.U(3.W)) // Register to store the opcode
      val sourceReg   = RegInit(0.U(4.W)) // Register to store the source ID
      val sizeReg     = RegInit(0.U(3.W)) // Register to store the size
      val paramReg    = RegInit(0.U(2.W)) // Register to store the parameter

      val transceiver = Module(new Transceiver) // Transceiver module instance

      // OX <-> Transceiver IO
      io.txdata := transceiver.io.txdata
      io.txvalid := transceiver.io.txvalid
      io.txlast := transceiver.io.txlast
      io.txkeep := transceiver.io.txkeep
      transceiver.io.txready := io.txready

      transceiver.io.rxdata := io.rxdata
      transceiver.io.rxvalid := io.rxvalid
      transceiver.io.rxlast := io.rxlast

      transceiver.io.ox_open := io.ox_open
      transceiver.io.ox_close := io.ox_close
      transceiver.io.debug1 := io.debug1
      transceiver.io.debug2 := io.debug2

      // Default values for the transceiver IO
      transceiver.io.txAddr   := 0.U
      transceiver.io.txData   := 0.U
      transceiver.io.txSize   := 0.U
      transceiver.io.txOpcode := 0.U
      transceiver.io.txValid  := false.B
      transceiver.io.rxReady  := false.B

      // When the input channel 'a' is ready and valid
      when (in.a.fire) { 
      // when (in.a.fire()) {
        // Transmit the address, data, and opcode from the input channel
        transceiver.io.txAddr   := in.a.bits.address
        transceiver.io.txData   := in.a.bits.data
        transceiver.io.txSize   := in.a.bits.size
        transceiver.io.txOpcode := in.a.bits.opcode

        // Store the opcode, source, size, and parameter for response
        opcodeReg := Mux(in.a.bits.opcode === TLMessages.Get, TLMessages.AccessAckData, TLMessages.AccessAck)
        sourceReg := in.a.bits.source
        sizeReg   := in.a.bits.size
        paramReg  := in.a.bits.param

        transceiver.io.txValid := true.B // Mark the transmission as valid
      }

      transceiver.io.rxReady := true.B // Always ready to receive data

      // Mark the input channel 'a' as valid
      when (in.a.valid) {
          aValidReg := true.B
      }


      // Default values for the response channel 'd'
      in.d.valid        := false.B
      in.d.bits.opcode  := 0.U
      in.d.bits.param   := 0.U
      in.d.bits.size    := 0.U
      in.d.bits.source  := 0.U
      in.d.bits.sink    := 0.U
      in.d.bits.denied  := false.B
      in.d.bits.data    := 0.U
      in.d.bits.corrupt := false.B


/*
      in.d.valid        := true.B
      in.d.bits.opcode  := opcodeReg
      in.d.bits.param   := paramReg
      in.d.bits.size    := sizeReg
      in.d.bits.source  := sourceReg
      in.d.bits.sink    := 0.U
      in.d.bits.denied  := false.B
      in.d.bits.data    := 0x55555555.U
      in.d.bits.corrupt := false.B
*/

      // When received data is not zero, prepare the response
      when (transceiver.io.axi_rxvalid) {                // RX valid signal received from Ethernet IP
          in.d.valid        := true.B                    // Mark the response as valid
          in.d.bits         := edge.AccessAck(in.a.bits) // Generate an AccessAck response
          in.d.bits.opcode  := opcodeReg                 // Set the opcode from the register
          in.d.bits.param   := paramReg                  // Set the parameter from the register
          in.d.bits.size    := sizeReg                   // Set the size from the register
          in.d.bits.source  := sourceReg                 // Set the source ID from the register
          in.d.bits.sink    := 0.U                       // Set sink to 0
          in.d.bits.denied  := false.B                   // Mark as not denied
   
        when (opcodeReg === TLMessages.AccessAckData) {
          switch (sizeReg) {
            is (1.U) { in.d.bits.data := transceiver.io.axi_rxdata(15, 0) } 

            is (2.U) { in.d.bits.data := transceiver.io.axi_rxdata(31, 0) }

            is (3.U) { in.d.bits.data := transceiver.io.axi_rxdata(63, 0) }

            is (4.U) { in.d.bits.data := transceiver.io.axi_rxdata(127, 0) }

            is (5.U) { in.d.bits.data := transceiver.io.axi_rxdata(255, 0) }

            is (6.U) { in.d.bits.data := transceiver.io.axi_rxdata }
          }
          in.d.bits.corrupt := false.B // Mark as not corrupt
        }.elsewhen (opcodeReg === TLMessages.AccessAck) {
          in.d.bits.data    := 0.U
        }
      }

      // Ready conditions for the input channel 'a' and response channel 'd'
      in.a.ready := in.a.valid || aValidReg
      in.d.ready := in.a.valid || aValidReg

      // IO ready signal is asserted when input is valid and opcode is Get or PutFullData
      io.ready := in.a.valid && (in.a.bits.opcode === TLMessages.Get || in.a.bits.opcode === TLMessages.PutFullData)
  }
}

trait OmniXtend { this: BaseSubsystem =>
  private val portName = "OmniXtend"
  implicit val p: Parameters

  val ox = LazyModule(new OmniXtendNode()(p))

  // define local mbus, as BaseSubsystem no longer contains the mbus 
  private val mbus = locateTLBusWrapper(MBUS)

  mbus.coupleTo(portName) { (ox.node
    :*= TLBuffer()
    :*= TLWidthWidget(mbus.beatBytes)
    :*= _)
  }

}

// trait OmniXtendModuleImp extends LazyModuleImp {
//   val outer: OmniXtend
//   implicit val p: Parameters

//   val io = IO(new OmniXtendBundle)

//   // io <> outer.ox.ext_io
//   io <> outer.ox.module.io
// }


class WithOX(useAXI4: Boolean = false, useBlackBox: Boolean = false) extends Config((site, here, up) => {
  case OXKey => Some(OXParams(useAXI4 = useAXI4, useBlackBox = useBlackBox))
})
