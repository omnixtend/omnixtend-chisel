package omnixtend

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import TLOEHeaderConstants._
import TLOESeqManagetConstant._

class TLOEReceiver extends Module {
  val io = IO(new Bundle {
    // Ethernet Interface
    val rxdata = Input(UInt(64.W))
    val rxvalid = Input(Bool())
    val rxlast = Input(Bool())

    // Sequence Management
    //val incTxSeq = Output(Bool())
    val incRxSeq = Output(Bool())
    val updateAckSeq = Output(Bool())
    val newAckSeq = Output(UInt(22.W))

    val nextTxSeq = Input(UInt(22.W))
    val nextRxSeq = Input(UInt(22.W))

    // Flow Control
    val incCreditValid = Output(Bool())
    val incCreditChannel = Output(UInt(3.W))
    val incCreditAmount = Output(UInt(5.W))

    val incAccCreditValid = Output(Bool())
    val incAccCreditChannel = Output(UInt(3.W))
    val incAccCreditAmount = Output(UInt(5.W))

    val credits = Input(Vec(6, UInt(16.W)))
    val accCredits = Input(Vec(6, UInt(16.W)))
    val error = Input(Bool())

    // Slide Window
    val slideValid = Output(Bool())
    val slideSeqNumAck = Output(UInt(22.W))
    val slideDone = Input(Bool())

    // Retransmission
    val retransmitDone = Input(Bool())
    val retransmitSeqNum = Output(UInt(22.W))
    val retransmitValid = Output(Bool())

    // Transfer ack info to Tx
    val ackSeqNum = Output(UInt(22.W))
    val ackType = Output(UInt(2.W))
    val ackReady = Output(Bool())
    val ackAckonly = Output(Bool())
    val ackAckonlyDone = Input(Bool())

    val epConn = Input(Bool())

    // TileLink Handler - reduced vector size
    val rxPacketVec = Output(Vec(34, UInt(64.W)))  // Reduced from 68 to 34
    val rxPacketVecSize = Output(UInt(8.W))
    val doTilelinkHandler = Output(Bool())
  })

  // TileLink Handler - reduced vector size
  io.rxPacketVec := VecInit(Seq.fill(34)(0.U(64.W)))  // Reduced from 68 to 34
  io.rxPacketVecSize := 0.U
  io.doTilelinkHandler := false.B

  // State registers - simplified
  val rxIdle :: rxPacketReceived :: rxSlideWindow :: rxRetransmission :: rxAckOnly :: rxCheckType :: rxFrameNormal :: rxHandleCredit :: rxFrameDup :: rxFrameOOS :: rxHandleAccCredit :: rxDone :: Nil = Enum(12)
  val rxState = RegInit(rxIdle)
  val rxComplete = RegInit(true.B)

  // Packet reception registers - reduced vector size
  val rxPacketVec = Reg(Vec(34, UInt(64.W)))  // Reduced from 68 to 34
  val rxPacketVecSize = RegInit(0.U(8.W))
  val rxCount = RegInit(0.U(8.W))
  val rxPacketEtherType = RegInit(0.U(16.W))

  val nextRxPacketVec = Reg(Vec(34, UInt(64.W)))  // Reduced from 68 to 34
  val nextRxPacketVecSize = RegInit(0.U(8.W))

  // Queue with reduced vector size
  val rxQueue = Module(new Queue(new Bundle {
    val rxPacketVec = Vec(34, UInt(64.W))  // Reduced from 68 to 34
    val rxPacketVecSize = UInt(8.W)
  }, 16))

  rxQueue.io.enq.valid := false.B
  rxQueue.io.enq.bits := 0.U.asTypeOf(rxQueue.io.enq.bits)
  rxQueue.io.deq.ready := false.B

  val tloeHeader = Reg(new tloeHeader)
  val tlHeader = Reg(new TLMessageHigh)
  val rxFrameMask = RegInit(0.U(64.W))

  val epConn = RegInit(false.B)
  epConn := io.epConn

  // Initialize outputs
  io.incRxSeq := false.B
  io.updateAckSeq := false.B
  io.newAckSeq := 0.U

  io.incCreditValid := false.B
  io.incCreditChannel := 0.U
  io.incCreditAmount := 0.U

  io.incAccCreditValid := false.B
  io.incAccCreditChannel := 0.U
  io.incAccCreditAmount := 0.U

  io.slideValid := false.B
  io.slideSeqNumAck := 0.U

  io.retransmitSeqNum := 0.U
  io.retransmitValid := false.B

  io.ackSeqNum := 0.U
  io.ackType := 0.U
  io.ackReady := false.B
  io.ackAckonly := false.B  

  // Request type enumeration
  object ReqType {
    val REQ_NORMAL = 0.U(2.W)
    val REQ_DUPLICATE = 1.U(2.W)
    val REQ_OOS = 2.U(2.W)  // Out of Sequence
  }

  // Function to determine request type based on sequence number comparison
  def getReqType(rxSeq: UInt, nextRxSeq: UInt): UInt = {
    val diff = TLOESeqManager.seqNumCompare(rxSeq, nextRxSeq)
    // Convert SInt to UInt for switch statement
    val diffUInt = Wire(UInt(2.W))
    when(diff === 0.S) {
      diffUInt := 0.U
    }.elsewhen(diff === (-1).S) {
      diffUInt := 1.U
    }.otherwise {
      diffUInt := 2.U
    }

    val reqType = WireDefault(ReqType.REQ_OOS)
    switch(diffUInt) {
      is(0.U) { reqType := ReqType.REQ_NORMAL }
      is(1.U) { reqType := ReqType.REQ_DUPLICATE }
      is(2.U) { reqType := ReqType.REQ_OOS }
    }
    reqType
  }

  when(io.ackAckonlyDone) {
    io.ackAckonly := false.B
  } 

  when (rxQueue.io.deq.valid && rxComplete) {
    nextRxPacketVec := rxQueue.io.deq.bits.rxPacketVec
    nextRxPacketVecSize := rxQueue.io.deq.bits.rxPacketVecSize
    rxQueue.io.deq.ready := true.B

    rxState := rxPacketReceived
    rxComplete := false.B
  }

  // State machine for packet reception
  switch(rxState) {
    is(rxIdle) {
    }

    is(rxPacketReceived) {
      val offset = PriorityEncoder(TloePacGen.getMask(nextRxPacketVec, nextRxPacketVecSize))

      // TLoE Header processing
      val tloeHeaderWire = Wire(new tloeHeader)
      tloeHeaderWire := Cat(
        TloePacGen.toBigEndian(nextRxPacketVec(1))(15, 0),
        TloePacGen.toBigEndian(nextRxPacketVec(2))(63, 16)
      ).asTypeOf(new tloeHeader)
      tloeHeader := tloeHeaderWire

      // TileLink Message processing
      val tlHeaderWire = Wire(new TLMessageHigh)
      tlHeaderWire := Cat(
        TloePacGen.toBigEndian(nextRxPacketVec(2.U +& offset))(15, 0),
        TloePacGen.toBigEndian(nextRxPacketVec(3.U +& offset))(63, 16)
      ).asTypeOf(new TLMessageHigh)
      tlHeader := tlHeaderWire

      rxFrameMask := TloePacGen.getMask(nextRxPacketVec, nextRxPacketVecSize)
      rxState := rxSlideWindow
    }

    is(rxSlideWindow) {
      // Serve Ack
      io.slideValid := true.B
      io.slideSeqNumAck := tloeHeader.seqNumAck

      when (io.slideDone) {
        rxState := rxRetransmission
      }
    }

    is(rxRetransmission) {
      // In case of NAK, retransmit the frame in the retransmit buffer
      when (tloeHeader.ack === TLOE_NAK) {
        io.retransmitValid := true.B
        io.retransmitSeqNum := tloeHeader.seqNumAck
      }.otherwise {
        rxState := rxAckOnly
      }

      when (io.retransmitDone) {
        rxState := rxAckOnly
      }
    }

    is(rxAckOnly) {
      when(tloeHeader.msgType === TLOE_TYPE_ACKONLY && tloeHeader.ack === TLOE_ACK) {
        io.newAckSeq := tloeHeader.seqNumAck
        io.updateAckSeq := true.B
        rxState := rxDone
      }.otherwise {
        rxState := rxCheckType
      }
    }

    is(rxCheckType) {
      val reqType = getReqType(tloeHeader.seqNum, io.nextRxSeq)

      switch(reqType) {
        is(ReqType.REQ_NORMAL) {
          rxState := rxFrameNormal
        }
        is(ReqType.REQ_DUPLICATE) {
          rxState := rxFrameDup
        }
        is(ReqType.REQ_OOS) {
          rxState := rxFrameOOS
        }
      }
    }

    is(rxFrameNormal) {
      when(rxFrameMask === 0.U) {
        // Zero-tl frame
        io.incRxSeq := true.B
        io.newAckSeq := tloeHeader.seqNumAck
        io.updateAckSeq := true.B

        rxState := rxHandleCredit
        
        io.ackAckonly := true.B
      }.otherwise {
        // Update nextRxSeq
        io.incRxSeq := true.B
        io.newAckSeq := tloeHeader.seqNumAck
        io.updateAckSeq := true.B

        // Set TileLink Handler signals
        io.rxPacketVec := nextRxPacketVec
        io.rxPacketVecSize := nextRxPacketVecSize
        io.doTilelinkHandler := true.B

        // Flow Control
        val rxRequiredFlits = TlMsgFlits.getFlitsCnt(tlHeader.chan, tlHeader.opcode, tlHeader.size)

        io.ackSeqNum := tloeHeader.seqNumAck
        io.ackType := TLOE_ACK
        io.ackReady := true.B

        rxState := rxHandleCredit
      }
    }

    is(rxFrameDup) {
      // Handle duplicate frame
      io.incRxSeq := true.B
      io.newAckSeq := tloeHeader.seqNumAck
      io.updateAckSeq := true.B

      io.ackSeqNum := tloeHeader.seqNumAck
      io.ackType := TLOE_ACK
      io.ackReady := true.B

      rxState := rxDone
    }

    is(rxFrameOOS) {
      // Handle out-of-sequence frame
      io.incRxSeq := true.B
      io.newAckSeq := tloeHeader.seqNumAck
      io.updateAckSeq := true.B

      io.ackSeqNum := tloeHeader.seqNumAck
      io.ackType := TLOE_ACK
      io.ackReady := true.B

      rxState := rxDone
    }

    is(rxHandleCredit) {
      // Handle credit increment
      io.incCreditChannel := tloeHeader.chan
      io.incCreditAmount := (1.U << tloeHeader.credit)
      io.incCreditValid := true.B

      rxState := rxHandleAccCredit
    }

    is(rxHandleAccCredit) {
      // Handle accumulated credit increment
      io.incAccCreditChannel := tloeHeader.chan
      io.incAccCreditAmount := (1.U << tloeHeader.credit)
      io.incAccCreditValid := true.B

      rxState := rxDone
    }

    is(rxDone) {
      rxComplete := true.B
      rxState := rxIdle
    }
  }

  // Packet reception logic
  when (io.rxvalid && !io.rxlast) {
    rxPacketVec(rxCount) := io.rxdata
    rxCount := rxCount + 1.U
  }

  when (io.rxvalid && io.rxlast) {
    rxPacketVec(rxCount) := io.rxdata
    rxPacketVecSize := rxCount + 1.U

    // Only enqueue if we have space and the packet is not too large
    when (rxQueue.io.enq.ready && (rxCount + 1.U) <= 34.U) {
      rxQueue.io.enq.valid := true.B
      rxQueue.io.enq.bits.rxPacketVec := rxPacketVec
      rxQueue.io.enq.bits.rxPacketVecSize := rxCount + 1.U
    }

    rxCount := 0.U
  }
} 

