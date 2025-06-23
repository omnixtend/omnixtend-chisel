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

    // TileLink Handler
    val rxPacketVec = Output(Vec(68, UInt(64.W)))
    val rxPacketVecSize = Output(UInt(8.W))
    val doTilelinkHandler = Output(Bool())
  })

  // TileLink Handler
  io.rxPacketVec := VecInit(Seq.fill(68)(0.U(64.W)))
  io.rxPacketVecSize := 0.U
  io.doTilelinkHandler := false.B

  // State registers
  val rxIdle :: rxPacketReceived :: rxSlideWindow :: rxRetransmission :: rxAckOnly :: rxCheckType :: rxFrameNormal :: rxHandleCredit :: rxFrameDup :: rxFrameOOS :: rxHandleAccCredit :: rxDone :: Nil = Enum(12)
  val rxState = dontTouch(RegInit(rxIdle))
  val rxComplete = dontTouch(RegInit(true.B))

  // Packet reception registers
  val rxPacketVec = dontTouch(Reg(Vec(68, UInt(64.W))))
  val rxPacketVecSize = dontTouch(RegInit(0.U(8.W)))
  val rxCount = dontTouch(RegInit(0.U(8.W)))
  val rxPacketEtherType = dontTouch(RegInit(0.U(16.W)))

  val nextRxPacketVec = dontTouch(Reg(Vec(68, UInt(64.W))))
  val nextRxPacketVecSize = dontTouch(RegInit(0.U(8.W)))

  val rxQueue = Module(new Queue(new Bundle {
    val rxPacketVec = Vec(68, UInt(64.W))
    val rxPacketVecSize = UInt(8.W)
  }, 16))

  rxQueue.io.enq.valid := false.B
  rxQueue.io.enq.bits := 0.U.asTypeOf(rxQueue.io.enq.bits)
  rxQueue.io.deq.ready := false.B

  val tloeHeader = dontTouch(Reg(new tloeHeader))
  val tlHeader = dontTouch(Reg(new TLMessageHigh))
  val rxFrameMask = dontTouch(RegInit(0.U(64.W)))

  val epConn = dontTouch(RegInit(false.B))
  epConn := io.epConn
  val do_tilelink_handler = dontTouch(RegInit(false.B))

  // debug
  val rxRequiredFlits = dontTouch(RegInit(0.U(8.W)))
  val incAccCreditValid = dontTouch(RegInit(false.B))
  val incAccCreditChannel = dontTouch(RegInit(0.U(3.W)))
  val incAccCreditAmount = dontTouch(RegInit(0.U(8.W)))

  //io.incTxSeq := false.B
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
    // Convert SInt to UInt for MuxLookup
    val diffUInt = Wire(UInt(2.W))
    when(diff === 0.S) {
      diffUInt := 0.U
    }.elsewhen(diff === (-1).S) {
      diffUInt := 1.U
    }.otherwise {
      diffUInt := 2.U
    }

    MuxLookup(diffUInt, ReqType.REQ_OOS, Seq(
      0.U -> ReqType.REQ_NORMAL,
      1.U -> ReqType.REQ_DUPLICATE,
      2.U -> ReqType.REQ_OOS
    ))
  }

  when(io.ackAckonlyDone) {
    io.ackAckonly := false.B
  } 

  val creditAIncCntDebug = dontTouch(RegInit(0.U(22.W)))

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
      val offset = PriorityEncoder(TloePacGen.getMask(nextRxPacketVec, nextRxPacketVecSize))  // Remove Reverse to search from LSB

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
        io.updateAckSeq := true.B  // Set updateAckSeq when ackdSeq is updated
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
        io.incRxSeq := true.B  // Set incRxSeq when incrementing RX sequence
        io.newAckSeq := tloeHeader.seqNumAck
        io.updateAckSeq := true.B

/*
        io.incCreditChannel := tloeHeader.chan
        io.incCreditAmount := (1.U << tloeHeader.credit)
        io.incCreditValid := true.B
        */

        rxState := rxHandleCredit
        
        // TODO tx에 Ackonly Frame 전송하도록 요청
        io.ackAckonly := true.B
      }.otherwise {
        // Update nextRxSeq
        io.incRxSeq := true.B
        io.newAckSeq := tloeHeader.seqNumAck
        io.updateAckSeq := true.B

        val doth_recv = dontTouch(RegInit(false.B))
        doth_recv := io.doTilelinkHandler

        //do_tilelink_handler := true.B
        io.rxPacketVec := nextRxPacketVec
        io.rxPacketVecSize := nextRxPacketVecSize
        io.doTilelinkHandler := true.B

        // Flow Control : decrease credit based on message type
        rxRequiredFlits := TlMsgFlits.getFlitsCnt(tlHeader.chan, tlHeader.opcode, tlHeader.size)

        // TODO tx에 Ackonly Frame 전송하도록 요청
        io.ackSeqNum := tloeHeader.seqNumAck
        io.ackType := TLOE_ACK
        io.ackReady := true.B

/*
        io.incCreditChannel := tloeHeader.chan
        io.incCreditAmount := (1.U << tloeHeader.credit)
        io.incCreditValid := true.B
        */

        rxState := rxHandleCredit
      }
    }

    is(rxHandleCredit) {
      // TODO check if chan is 0
      io.incCreditChannel := tloeHeader.chan
      io.incCreditAmount := (1.U << tloeHeader.credit)
      io.incCreditValid := true.B
      creditAIncCntDebug := creditAIncCntDebug + (1.U << tloeHeader.credit)

      when(rxFrameMask === 0.U) {
        rxState := rxDone
      }.otherwise {
        rxState := rxHandleAccCredit
      }
    }

    is(rxFrameDup) {
      // TODO tx에 Ackonly Frame 전송하도록 요청
      io.ackSeqNum := tloeHeader.seqNumAck
      io.ackType := TLOE_ACK
      io.ackReady := true.B

      rxState := rxDone
    }

    is(rxFrameOOS) {
      // TODO tx에 Ackonly Frame 전송하도록 요청
      io.ackSeqNum := TLOESeqManager.getPrevSeq(io.nextRxSeq)
      io.ackType := TLOE_NAK
      io.ackReady := true.B

      rxState := rxDone
    }

    is(rxHandleAccCredit) {
      io.incAccCreditChannel := tlHeader.chan
      io.incAccCreditAmount := rxRequiredFlits
      io.incAccCreditValid := true.B

      rxState := rxDone
      /*
      val creditUpdateInProgress = dontTouch(RegInit(false.B))
      
      when(!creditUpdateInProgress) {
        io.incAccCreditValid := true.B
        io.incAccCreditChannel := tlHeader.chan
        io.incAccCreditAmount := rxRequiredFlits
        creditUpdateInProgress := true.B
        rxState := rxDone
      }.otherwise {
        io.incAccCreditValid := false.B
        creditUpdateInProgress := false.B
        rxState := rxHandleAccCredit
      }
      */
    }

    is(rxDone) {
      rxState := rxIdle
      rxComplete := true.B
    }
  }

  when(rxState === rxDone) {
    rxState := rxIdle
    rxComplete := true.B
  }

  val rxdata_rev = dontTouch(RegInit(0.U(64.W)))
  val rxvalid_rev = dontTouch(RegInit(false.B))
  val rxlast_rev = dontTouch(RegInit(false.B))
  rxdata_rev := io.rxdata
  rxvalid_rev := io.rxvalid
  rxlast_rev := io.rxlast  

  val rxDropCntDebug = dontTouch(RegInit(0.U(22.W)))

  //////////////////////////////////////////////////////////////////
  // RX - Receive Path for AXI-Stream Data

  // If io.rxvalid signal is low, indicating no incoming data, reset rxcount
  when(!io.rxvalid) {
    rxCount := 0.U
  }

  // Debug signals for rxQueue
  val rxQueueDebug = dontTouch(RegInit(0.U(4.W)))
  val rxQueueEnqValid = dontTouch(RegInit(false.B))
  val rxQueueEnqReady = dontTouch(RegInit(false.B))
  val rxQueueDeqValid = dontTouch(RegInit(false.B))
  val rxQueueDeqReady = dontTouch(RegInit(false.B))
  val rxQueueCount = dontTouch(RegInit(0.U(5.W)))

  rxQueueEnqValid := rxQueue.io.enq.valid
  rxQueueEnqReady := rxQueue.io.enq.ready
  rxQueueDeqValid := rxQueue.io.deq.valid
  rxQueueDeqReady := rxQueue.io.deq.ready
  rxQueueCount := rxQueue.io.count

  val rxReceiveDone = dontTouch(RegInit(false.B))

  // When io.rxvalid is high, data is being received
  when(io.rxvalid) {
    rxCount := rxCount + 1.U

    // Store incoming data at the current rxcount index in rPacketVec vector
    rxPacketVec(rxCount) := io.rxdata

    // Check if io.rxlast is high, signaling the end of the packet
    when(io.rxlast) {
      rxReceiveDone := true.B
    }
  }

  when (rxReceiveDone) {
    val etherPacketSize = rxCount
    val etherType = TloePacGen.getEtherType(rxPacketVec)

    when(io.epConn && etherType === 0xAAAA.U) {
      when(rxQueue.io.enq.ready) {    
        rxQueue.io.enq.bits.rxPacketVec := rxPacketVec
        rxQueue.io.enq.bits.rxPacketVecSize := etherPacketSize
        rxQueue.io.enq.valid := true.B
        rxQueueDebug := 1.U  // Successfully enqueued
      }.otherwise {  // Queue is full
        rxDropCntDebug := rxDropCntDebug + 1.U
        rxQueueDebug := 2.U  // Queue full
      } 
    }.otherwise {  // Not EP connection or not AAAA frame
      rxQueueDebug := Mux(io.epConn, 3.U, 4.U)  // 3: Wrong etherType, 4: Not EP connection
    }

    rxReceiveDone := false.B
  }
} 
