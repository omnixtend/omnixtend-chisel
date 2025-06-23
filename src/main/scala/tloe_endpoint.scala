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

    // TileLink Handler signals - reduced vector size
    val rxPacketVec = Output(Vec(34, UInt(64.W)))  // Reduced from 68 to 34
    val rxPacketVecSize = Output(UInt(8.W))
    val doTilelinkHandler = Output(Bool())
  })

  // Core state registers - only essential ones
  val isRetransmit = RegInit(false.B)
  val isConn = RegInit(false.B)

  // Core modules
  val transmitter    = Module(new TLOETransmitter)
  val receiver       = Module(new TLOEReceiver)
  val tloeSeqNum     = Module(new TLOESeqManager)
  val flowControl    = Module(new FlowControl)
  val timer          = Module(new GlobalTimer)
  val retransmission = Module(new Retransmission)
  val connection     = Module(new TloeConnection)

  // Direct connections to TileLink Handler - with size reduction
  io.rxPacketVec := receiver.io.rxPacketVec.take(34)  // Only take first 34 elements
  io.rxPacketVecSize := receiver.io.rxPacketVecSize
  io.doTilelinkHandler := receiver.io.doTilelinkHandler

  // Optimized output multiplexing
  io.txdata := Mux(!isConn, connection.io.txdata,
    Mux(isRetransmit, retransmission.io.txdata, transmitter.io.txdata))
  io.txvalid := Mux(!isConn, connection.io.txvalid,
    Mux(isRetransmit, retransmission.io.txvalid, transmitter.io.txvalid))
  io.txlast := Mux(!isConn, connection.io.txlast,
    Mux(isRetransmit, retransmission.io.txlast, transmitter.io.txlast))
  io.txkeep := Mux(!isConn, connection.io.txkeep,
    Mux(isRetransmit, retransmission.io.txkeep, transmitter.io.txkeep))

  // Sequence Manager connections - simplified
  tloeSeqNum.io.reset := false.B
  tloeSeqNum.io.incTxSeq := Mux(isConn, transmitter.io.incTxSeq, connection.io.incTxSeq)
  tloeSeqNum.io.incRxSeq := Mux(isConn, receiver.io.incRxSeq, connection.io.incRxSeq)
  tloeSeqNum.io.updateAckSeq := Mux(isConn, receiver.io.updateAckSeq, connection.io.updateAckSeq)
  tloeSeqNum.io.newAckSeq := Mux(isConn, receiver.io.newAckSeq, connection.io.newAckSeq)

  // Sequence number distribution
  transmitter.io.nextTxSeq := tloeSeqNum.io.nextTxSeq
  transmitter.io.nextRxSeq := tloeSeqNum.io.nextRxSeq
  transmitter.io.ackdSeq := tloeSeqNum.io.ackdSeq

  receiver.io.nextTxSeq := tloeSeqNum.io.nextTxSeq
  receiver.io.nextRxSeq := tloeSeqNum.io.nextRxSeq

  connection.io.nextTxSeq := tloeSeqNum.io.nextTxSeq
  connection.io.nextRxSeq := tloeSeqNum.io.nextRxSeq
  connection.io.ackdSeq := tloeSeqNum.io.ackdSeq

  // TileLink interface to transmitter
  transmitter.io.txOpcode := io.txOpcode
  transmitter.io.txParam := io.txParam
  transmitter.io.txSize := io.txSize
  transmitter.io.txSource := io.txSource
  transmitter.io.txAddr := io.txAddr
  transmitter.io.txData := io.txData
  transmitter.io.txMask := io.txMask
  transmitter.io.txValid := io.txValid

  // Optimized receiver/connection data routing
  when (!isConn) {
    connection.io.txready := io.txready
    connection.io.rxdata := io.rxdata
    connection.io.rxvalid := io.rxvalid
    connection.io.rxlast := io.rxlast

    receiver.io.rxdata := 0.U(64.W)
    receiver.io.rxvalid := false.B
    receiver.io.rxlast := false.B
  }.otherwise {
    receiver.io.rxdata := io.rxdata
    receiver.io.rxvalid := io.rxvalid
    receiver.io.rxlast := io.rxlast

    connection.io.txready := false.B
    connection.io.rxdata := 0.U(64.W)
    connection.io.rxvalid := false.B
    connection.io.rxlast := false.B
  }

  // Receiver connections
  receiver.io.nextRxSeq := tloeSeqNum.io.nextRxSeq
  receiver.io.slideDone := retransmission.io.slideDone
  receiver.io.retransmitDone := retransmission.io.retransmitDone

  // Flow Control - simplified connections
  flowControl.io.decCredit.valid := transmitter.io.decCreditValid
  flowControl.io.decCredit.channel := transmitter.io.decCreditChannel
  flowControl.io.decCredit.credit := transmitter.io.decCreditAmount
  flowControl.io.decAccCredit.valid := transmitter.io.decAccCreditValid
  flowControl.io.decAccCredit.channel := transmitter.io.decAccCreditChannel
  flowControl.io.decAccCredit.credit := transmitter.io.decAccCreditAmount

  // Flow control to transmitter
  transmitter.io.credits := flowControl.io.credits
  transmitter.io.accCredits := flowControl.io.accCredits
  transmitter.io.error := flowControl.io.error
  transmitter.io.maxCreditChannel := flowControl.io.maxCreditChannel
  transmitter.io.maxCredit := flowControl.io.maxCredit

  // Flow control from receiver/connection
  flowControl.io.incCredit.valid := Mux(isConn, receiver.io.incCreditValid, connection.io.incCreditValid)
  flowControl.io.incCredit.channel := Mux(isConn, receiver.io.incCreditChannel, connection.io.incCreditChannel)
  flowControl.io.incCredit.credit := Mux(isConn, receiver.io.incCreditAmount, connection.io.incCreditAmount)

  flowControl.io.incAccCredit.valid := receiver.io.incAccCreditValid
  flowControl.io.incAccCredit.channel := receiver.io.incAccCreditChannel
  flowControl.io.incAccCredit.credit := receiver.io.incAccCreditAmount
  
  // Flow control to receiver
  receiver.io.credits := flowControl.io.credits
  receiver.io.accCredits := flowControl.io.accCredits
  receiver.io.error := flowControl.io.error

  // Global Timer
  timer.io.resetTimer := false.B
  transmitter.io.currTime := timer.io.globalTimer
  retransmission.io.currTime := timer.io.globalTimer

  // Retransmission connections
  retransmission.io.clear := false.B
  retransmission.io.retransmitSeqNum := receiver.io.retransmitSeqNum
  retransmission.io.retransmitValid := receiver.io.retransmitValid
  retransmission.io.slideSeqNumAck := receiver.io.slideSeqNumAck
  retransmission.io.slideValid := receiver.io.slideValid

  retransmission.io.read := 0.U(896.W)
  retransmission.io.write := transmitter.io.retransmitWrite
  retransmission.io.writeValid := transmitter.io.retransmitWriteValid
  transmitter.io.retransmitIsFull := retransmission.io.isFull
  transmitter.io.isRetransmit := retransmission.io.isRetransmit

  isRetransmit := retransmission.io.isRetransmit

  // Connection management
  connection.io.oxOpen := io.ox_open
  connection.io.oxClose := io.ox_close
  connection.io.reset := false.B

  isConn := connection.io.isConn
  transmitter.io.epConn := connection.io.isConn
  receiver.io.epConn := connection.io.isConn

  // Transmitter-receiver communication
  transmitter.io.ackSeqNum := receiver.io.ackSeqNum
  transmitter.io.ackType := receiver.io.ackType
  transmitter.io.ackReady := receiver.io.ackReady
  transmitter.io.ackAckonly := receiver.io.ackAckonly
  receiver.io.ackAckonlyDone := transmitter.io.ackAckonlyDone

  // Debug signals
  transmitter.io.debug1 := io.ox_debug1
  transmitter.io.debug2 := io.ox_debug2
} 
