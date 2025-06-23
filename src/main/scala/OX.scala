package omnixtend

import chisel3._
import chisel3.util._

import chisel3.experimental.{IntParam, BaseModule}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf

case class OXParams(
  address: BigInt = 0x1000,
  width: Int = 32,
  useAXI4: Boolean = false,
  useBlackBox: Boolean = true
)

case object OXKey extends Field[Option[OXParams]](None)

// Definition of OmniXtend Bundle
class OmniXtendBundle extends Bundle {
  val addr        = Input(UInt(64.W))
  val valid       = Input(Bool())   // signals if the transaction is valid
  val ready       = Output(Bool())  // signals if the transaction can proceed
  val in          = Input(UInt(8.W))

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

  /*
  val isConn      = Output(Bool())
  val maxCredit   = Output(UInt(5.W))  // maxCredit 포트 추가
  val globalTimer = Output(UInt(64.W))  // Add global timer output
  */
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

  lazy val module = new LazyModuleImp(this) {
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

    val TLOEEndpoint = Module(new TLOEEndpoint)
    val tilelinkHandler = Module(new TileLinkHandler)

    val rxPacketVec_ox = dontTouch(Reg(Vec(68, UInt(64.W))))
    val rxPacketVecSize_ox = dontTouch(RegInit(0.U(8.W)))
    val doTilelinkHandler_ox = dontTouch(RegInit(false.B))
    rxPacketVec_ox := TLOEEndpoint.io.rxPacketVec
    rxPacketVecSize_ox := TLOEEndpoint.io.rxPacketVecSize
    doTilelinkHandler_ox := TLOEEndpoint.io.doTilelinkHandler

    tilelinkHandler.io.rxPacketVec := TLOEEndpoint.io.rxPacketVec
    tilelinkHandler.io.rxPacketVecSize := TLOEEndpoint.io.rxPacketVecSize
    tilelinkHandler.io.doTilelinkHandler := TLOEEndpoint.io.doTilelinkHandler

    // Connect shared IO signals using multiplexers
    io.txdata := TLOEEndpoint.io.txdata
    io.txvalid := TLOEEndpoint.io.txvalid
    io.txlast := TLOEEndpoint.io.txlast
    io.txkeep := TLOEEndpoint.io.txkeep
    
    TLOEEndpoint.io.txready := io.txready
    TLOEEndpoint.io.rxdata := io.rxdata
    TLOEEndpoint.io.rxvalid := io.rxvalid
    TLOEEndpoint.io.rxlast := io.rxlast

    TLOEEndpoint.io.txAddr := 0.U
    TLOEEndpoint.io.txData := 0.U
    TLOEEndpoint.io.txSize := 0.U
    TLOEEndpoint.io.txOpcode := 0.U
    TLOEEndpoint.io.txValid := false.B
    TLOEEndpoint.io.txMask := 0.U
    TLOEEndpoint.io.txSource := 0.U
    TLOEEndpoint.io.txParam := 0.U

    TLOEEndpoint.io.rxdata := io.rxdata
    TLOEEndpoint.io.rxvalid := io.rxvalid
    TLOEEndpoint.io.rxlast := io.rxlast

    val rxdata_ox = dontTouch(RegInit(0.U(512.W)))
    val rxvalid_ox = dontTouch(RegInit(false.B))  
    val rxlast_ox = dontTouch(RegInit(false.B))
    rxdata_ox := TLOEEndpoint.io.rxdata
    rxvalid_ox := TLOEEndpoint.io.rxvalid
    rxlast_ox := TLOEEndpoint.io.rxlast

    // VIO
    TLOEEndpoint.io.ox_open := io.ox_open
    TLOEEndpoint.io.ox_close := io.ox_close
    TLOEEndpoint.io.ox_debug1 := io.debug1
    TLOEEndpoint.io.ox_debug2 := io.debug2

    // When the input channel 'a' is ready and valid
    when (in.a.fire()) {
      // Transmit the address, data, and opcode from the input channel
      TLOEEndpoint.io.txAddr   := in.a.bits.address
      TLOEEndpoint.io.txData   := in.a.bits.data
      TLOEEndpoint.io.txSize   := in.a.bits.size
      TLOEEndpoint.io.txOpcode := in.a.bits.opcode
      TLOEEndpoint.io.txMask   := in.a.bits.mask
      TLOEEndpoint.io.txSource := in.a.bits.source
      TLOEEndpoint.io.txParam  := in.a.bits.param

      // Store the opcode, source, size, and parameter for response
      opcodeReg := Mux(in.a.bits.opcode === TLMessages.Get, TLMessages.AccessAckData, TLMessages.AccessAck)
      sourceReg := in.a.bits.source
      sizeReg   := in.a.bits.size
      paramReg  := in.a.bits.param

      TLOEEndpoint.io.txValid := true.B // Mark the transmission as valid
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

    //Debug
    val dValid = dontTouch(RegInit(false.B))
    val dOpcode = dontTouch(RegInit(0.U(3.W)))
    val dParam = dontTouch(RegInit(0.U(2.W)))
    val dSize = dontTouch(RegInit(0.U(3.W)))
    val dSource = dontTouch(RegInit(0.U(4.W)))
    val dSink = dontTouch(RegInit(0.U(4.W)))
    val dDenied = dontTouch(RegInit(false.B))
    val dData = dontTouch(RegInit(0.U(512.W)))

    dValid := in.d.valid
    dOpcode := in.d.bits.opcode
    dParam := in.d.bits.param
    dSize := in.d.bits.size
    dSource := in.d.bits.source
    dSink := in.d.bits.sink
    dDenied := in.d.bits.denied
    dData := in.d.bits.data

    val ep_rxData = dontTouch(RegInit(0.U(512.W)))
    val ep_rxValid = dontTouch(RegInit(false.B))
    ep_rxData := tilelinkHandler.io.ep_rxData
    ep_rxValid := tilelinkHandler.io.ep_rxValid

    val oxResponse = dontTouch(RegInit(false.B))

    val tlMsgOpcode = dontTouch(RegInit(0.U(3.W)))
    val tlMsgParam = dontTouch(RegInit(0.U(4.W)))
    val tlMsgSize = dontTouch(RegInit(0.U(4.W)))
    val tlMsgSource = dontTouch(RegInit(0.U(26.W)))
    val tlMsgData = dontTouch(RegInit(0.U(512.W)))

    // When received data is not zero, prepare the response
    //when (TLOEEndpoint.io.ep_rxvalid) {                // RX valid signal received from Ethernet IP
    when (tilelinkHandler.io.ep_rxValid) {                // RX valid signal received from Ethernet IP
      in.d.valid        := true.B                    // Mark the response as valid

      tlMsgOpcode := tilelinkHandler.io.ep_rxOpcode
      tlMsgParam := tilelinkHandler.io.ep_rxParam
      tlMsgSize := tilelinkHandler.io.ep_rxSize
      tlMsgSource := tilelinkHandler.io.ep_rxSource
      tlMsgData := tilelinkHandler.io.ep_rxData

      oxResponse := true.B
    }

    when (oxResponse) {
        in.d.valid        := true.B                    // Mark the response as valid
        in.d.bits         := edge.AccessAck(in.a.bits) // Generate an AccessAck response
        in.d.bits.opcode  := tlMsgOpcode                 // Set the opcode from the register
        in.d.bits.param   := tlMsgParam                  // Set the parameter from the register
        in.d.bits.size    := tlMsgSize                   // Set the size from the register
        in.d.bits.source  := tlMsgSource                 // Set the source ID from the register
        in.d.bits.sink    := 0.U                       // Set sink to 0
        in.d.bits.denied  := false.B                   // Mark as not denied
 
      when (tlMsgOpcode === TLMessages.AccessAckData) {
        switch (tlMsgSize) {
          is (1.U) { in.d.bits.data := tlMsgData(15, 0) } 

          is (2.U) { in.d.bits.data := tlMsgData(31, 0) }

          is (3.U) { in.d.bits.data := tlMsgData(63, 0) }

          is (4.U) { in.d.bits.data := tlMsgData(127, 0) }

          is (5.U) { in.d.bits.data := tlMsgData(255, 0) }

          is (6.U) { in.d.bits.data := tlMsgData }
        }
        in.d.bits.corrupt := false.B // Mark as not corrupt
      }.elsewhen (tlMsgOpcode === TLMessages.AccessAck) {
        in.d.bits.data    := 0.U
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

// OmniXtend Trait
trait OmniXtend { this: BaseSubsystem =>
  private val portName = "OmniXtend"
  implicit val p: Parameters

  val ox = LazyModule(new OmniXtendNode()(p))

  mbus.coupleTo(portName) { (ox.node
    :*= TLBuffer()
    :*= TLWidthWidget(mbus.beatBytes)
    :*= _)
  }
}

// OmniXtendModuleImp Trait
trait OmniXtendModuleImp extends LazyModuleImp {
  val outer: OmniXtend
  implicit val p: Parameters

  val io = IO(new OmniXtendBundle)

  io <> outer.ox.module.io
}

class WithOX(useAXI4: Boolean = false, useBlackBox: Boolean = false) extends Config((site, here, up) => {
  case OXKey => Some(OXParams(useAXI4 = useAXI4, useBlackBox = useBlackBox))
})
