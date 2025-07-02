package omnixtend

import chisel3._
import chisel3.util._

class TLOEEndpoint extends Module {
    val io = IO(new Bundle {
    // TileLink Interface
    val txOpcode = Input(UInt(3.W))
    val txParam = Input(UInt(4.W))
    val txSize = Input(UInt(4.W))
    val txSource = Input(UInt(26.W))
    val txAddr = Input(UInt(64.W))
    val txData = Input(UInt(512.W))
    val txMask = Input(UInt(64.W))
    val txValid = Input(Bool())

    // Ethernet Interface
    val txdata = Output(UInt(64.W))
    val txvalid = Output(Bool())
    val txlast = Output(Bool())
    val txkeep = Output(UInt(8.W))

    val txready = Input(Bool())
    val rxdata = Input(UInt(512.W))
    val rxvalid = Input(Bool())
    val rxlast = Input(Bool())

    // VIO
    val ox_open = Input(Bool())
    val ox_close = Input(Bool())
    val ox_debug1 = Input(UInt(64.W))
    val ox_debug2 = Input(UInt(64.W))

    // TileLink Handler signals
    val rxPacketVec = Output(Vec(68, UInt(64.W)))
    val rxPacketVecSize = Output(UInt(8.W))
    val doTilelinkHandler = Output(Bool())
  })

  val isRetransmit = RegInit(false.B)
  //val isConn = RegInit(false.B)
  val isConn = RegInit(true.B)

/* debug
  val rxdata_ep = RegInit(0.U(64.W))
  val rxvalid_ep = RegInit(false.B)
  val rxlast_ep = RegInit(false.B)
  rxdata_ep := io.rxdata
  rxvalid_ep := io.rxvalid
  rxlast_ep := io.rxlast
  */

  val transmitter    = Module(new TLOETransmitter)
  val receiver       = Module(new TLOEReceiver)
  val tloeSeqNum     = Module(new TLOESeqManager)
//  val flowControl    = Module(new FlowControl)
//  val timer          = Module(new GlobalTimer)
//  val retransmission = Module(new Retransmission)
//  val connection     = Module(new TloeConnection)

  val doth_ep = RegInit(false.B)
  doth_ep := receiver.io.doTilelinkHandler

  io.rxPacketVec := receiver.io.rxPacketVec
  io.rxPacketVecSize := receiver.io.rxPacketVecSize
  io.doTilelinkHandler := receiver.io.doTilelinkHandler

  /* Debug
  val d_episConn = dontTouch(RegInit(true.B))
  d_episConn := isConn
  */

  io.txdata := transmitter.io.txdata
  io.txvalid := transmitter.io.txvalid
  io.txlast := transmitter.io.txlast
  io.txkeep := transmitter.io.txkeep

  // Sequence Manager
  tloeSeqNum.io.reset := false.B  // Reset is handled by the node's reset
  tloeSeqNum.io.incTxSeq := transmitter.io.incTxSeq
  tloeSeqNum.io.incRxSeq := receiver.io.incRxSeq
  tloeSeqNum.io.updateAckSeq := receiver.io.updateAckSeq
  tloeSeqNum.io.newAckSeq := receiver.io.newAckSeq

  transmitter.io.nextTxSeq := tloeSeqNum.io.nextTxSeq
  transmitter.io.nextRxSeq := tloeSeqNum.io.nextRxSeq
  transmitter.io.ackdSeq := tloeSeqNum.io.ackdSeq

  receiver.io.nextTxSeq := tloeSeqNum.io.nextTxSeq
  receiver.io.nextRxSeq := tloeSeqNum.io.nextRxSeq
  transmitter.io.txOpcode := io.txOpcode
  transmitter.io.txParam := io.txParam
  transmitter.io.txSize := io.txSize
  transmitter.io.txSource := io.txSource
  transmitter.io.txAddr := io.txAddr
  transmitter.io.txData := io.txData
  transmitter.io.txMask := io.txMask
  transmitter.io.txValid := io.txValid

  receiver.io.rxdata := io.rxdata
  receiver.io.rxvalid := io.rxvalid
  receiver.io.rxlast := io.rxlast

  receiver.io.nextRxSeq := tloeSeqNum.io.nextRxSeq
  //receiver.io.slideDone := retransmission.io.slideDone
  //receiver.io.retransmitDone := retransmission.io.retransmitDone
  receiver.io.slideDone := false.B
  receiver.io.retransmitDone := false.B

  // Sequence Management
  tloeSeqNum.io.reset := false.B

  // Flow Control
  // TODO MUX???
  /*
  flowControl.io.decCredit.valid := transmitter.io.decCreditValid
  flowControl.io.decCredit.channel := transmitter.io.decCreditChannel
  flowControl.io.decCredit.credit := transmitter.io.decCreditAmount
  flowControl.io.decAccCredit.valid := transmitter.io.decAccCreditValid
  flowControl.io.decAccCredit.channel := transmitter.io.decAccCreditChannel
  flowControl.io.decAccCredit.credit := transmitter.io.decAccCreditAmount
  */

  // TODO 사용하는지??? credits, accCredits, error
// Connect flow control signals to transceiver
  /*
  transmitter.io.credits := flowControl.io.credits
  transmitter.io.accCredits := flowControl.io.accCredits
  transmitter.io.error := flowControl.io.error
  transmitter.io.maxCreditChannel := flowControl.io.maxCreditChannel
  transmitter.io.maxCredit := flowControl.io.maxCredit
  */

  // Flow Control
  /*
  flowControl.io.incCredit.valid := receiver.io.incCreditValid
  flowControl.io.incCredit.channel := receiver.io.incCreditChannel
  flowControl.io.incCredit.credit := receiver.io.incCreditAmount

  flowControl.io.incAccCredit.valid := receiver.io.incAccCreditValid
  flowControl.io.incAccCredit.channel := receiver.io.incAccCreditChannel
  flowControl.io.incAccCredit.credit := receiver.io.incAccCreditAmount
  */
  
  // TODO 사용하는지??? credits, accCredits, error
  /*
  receiver.io.credits := flowControl.io.credits
  receiver.io.accCredits := flowControl.io.accCredits
  receiver.io.error := flowControl.io.error
  */

  // Global Timer
  //timer.io.resetTimer := false.B
  //transmitter.io.currTime := timer.io.globalTimer
  //retransmission.io.currTime := timer.io.globalTimer

  // Retransmission
  //retransmission.io.clear := false.B
  //retransmission.io.retransmitSeqNum := receiver.io.retransmitSeqNum
  //retransmission.io.retransmitValid := receiver.io.retransmitValid
  //retransmission.io.slideSeqNumAck := receiver.io.slideSeqNumAck
  //retransmission.io.slideValid := receiver.io.slideValid

  //retransmission.io.read := 0.U(896.W)
  //retransmission.io.write := transmitter.io.retransmitWrite
  //retransmission.io.writeValid := transmitter.io.retransmitWriteValid
  //transmitter.io.retransmitIsFull := retransmission.io.isFull
  //transmitter.io.isRetransmit := retransmission.io.isRetransmit

  //isRetransmit := retransmission.io.isRetransmit

  isConn := RegInit(true.B)
  transmitter.io.epConn := isConn
  receiver.io.epConn := isConn

  // transmitter <> receiver
  transmitter.io.ackSeqNum := receiver.io.ackSeqNum
  transmitter.io.ackType := receiver.io.ackType
  transmitter.io.ackReady := receiver.io.ackReady
  transmitter.io.ackAckonly := receiver.io.ackAckonly
  receiver.io.ackAckonlyDone := transmitter.io.ackAckonlyDone

  transmitter.io.debug1 := io.ox_debug1
  transmitter.io.debug2 := io.ox_debug2
} 