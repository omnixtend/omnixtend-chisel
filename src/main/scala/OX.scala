package omnixtend

import chisel3._
import chisel3.util._

import chisel3.experimental.{IntParam, BaseModule}
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
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
  val ready       = Output(Bool())  // signals if the transaction can proceed

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
class OmniXtendNode(implicit p: Parameters) extends LazyModule {
  val beatBytes = 64 // The size of each data beat in bytes
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address            = Seq(AddressSet(0x500000000L, 0x01FFFFFFL)), // Address range this node responds to
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

    // Core modules
    val TLOEEndpoint = Module(new TLOEEndpoint)
    val tilelinkHandler = Module(new TileLinkHandler)

    // State management - simplified
    val aValidReg = RegInit(false.B)
    val oxResponse = RegInit(false.B)

    // Response data registers - only keep essential ones
    val tlMsgOpcode = RegInit(0.U(3.W))
    val tlMsgParam = RegInit(0.U(4.W))
    val tlMsgSize = RegInit(0.U(4.W))
    val tlMsgSource = RegInit(0.U(26.W))
    val tlMsgData = RegInit(0.U(512.W))

    // Connect TLOEEndpoint to TileLinkHandler
    tilelinkHandler.io.rxPacketVec := TLOEEndpoint.io.rxPacketVec
    tilelinkHandler.io.rxPacketVecSize := TLOEEndpoint.io.rxPacketVecSize
    tilelinkHandler.io.doTilelinkHandler := TLOEEndpoint.io.doTilelinkHandler

    // Connect shared IO signals
    io.txdata := TLOEEndpoint.io.txdata
    io.txvalid := TLOEEndpoint.io.txvalid
    io.txlast := TLOEEndpoint.io.txlast
    io.txkeep := TLOEEndpoint.io.txkeep
    
    TLOEEndpoint.io.txready := io.txready
    TLOEEndpoint.io.rxdata := io.rxdata
    TLOEEndpoint.io.rxvalid := io.rxvalid
    TLOEEndpoint.io.rxlast := io.rxlast

    // Initialize TLOEEndpoint inputs
    TLOEEndpoint.io.txAddr := 0.U
    TLOEEndpoint.io.txData := 0.U
    TLOEEndpoint.io.txSize := 0.U
    TLOEEndpoint.io.txOpcode := 0.U
    TLOEEndpoint.io.txValid := false.B
    TLOEEndpoint.io.txMask := 0.U
    TLOEEndpoint.io.txSource := 0.U
    TLOEEndpoint.io.txParam := 0.U

    // VIO connections
    TLOEEndpoint.io.ox_open := io.ox_open
    TLOEEndpoint.io.ox_close := io.ox_close
    TLOEEndpoint.io.ox_debug1 := io.debug1
    TLOEEndpoint.io.ox_debug2 := io.debug2

    // Handle input channel 'a' - simplified logic
    when (in.a.fire) {
      // Transmit the address, data, and opcode from the input channel
      TLOEEndpoint.io.txAddr   := in.a.bits.address
      TLOEEndpoint.io.txData   := in.a.bits.data
      TLOEEndpoint.io.txSize   := in.a.bits.size
      TLOEEndpoint.io.txOpcode := in.a.bits.opcode
      TLOEEndpoint.io.txMask   := in.a.bits.mask
      TLOEEndpoint.io.txSource := in.a.bits.source
      TLOEEndpoint.io.txParam  := in.a.bits.param
      TLOEEndpoint.io.txValid  := true.B
    }

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

    // Handle response from TileLinkHandler - simplified
    when (tilelinkHandler.io.ep_rxValid) {
      tlMsgOpcode := tilelinkHandler.io.ep_rxOpcode
      tlMsgParam := tilelinkHandler.io.ep_rxParam
      tlMsgSize := tilelinkHandler.io.ep_rxSize
      tlMsgSource := tilelinkHandler.io.ep_rxSource
      tlMsgData := tilelinkHandler.io.ep_rxData
      oxResponse := true.B
    }

    // Generate response - optimized data selection
    when (oxResponse) {
      in.d.valid := true.B
      in.d.bits.opcode := tlMsgOpcode
      in.d.bits.param := tlMsgParam
      in.d.bits.size := tlMsgSize
      in.d.bits.source := tlMsgSource
      in.d.bits.sink := 0.U
      in.d.bits.denied := false.B
      
      // Optimized data selection using bit shifting instead of switch
      when (tlMsgOpcode === TLMessages.AccessAckData) {
        // Use bit shifting for data selection - more efficient than switch
        val dataShift = (1.U << tlMsgSize) - 1.U
        in.d.bits.data := (tlMsgData & dataShift)(63, 0)
        in.d.bits.corrupt := false.B
      }.otherwise {
        in.d.bits.data := 0.U
      }
      oxResponse := false.B
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

class WithOX(useAXI4: Boolean = false, useBlackBox: Boolean = false) extends Config((site, here, up) => {
  case OXKey => Some(OXParams(useAXI4 = useAXI4, useBlackBox = useBlackBox))
})
