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
    val incRxSeq = Output(Bool())
    val updateAckSeq = Output(Bool())
    val newAckSeq = Output(UInt(22.W))

    val nextTxSeq = Input(UInt(22.W))
    val nextRxSeq = Input(UInt(22.W))

    // Flow Control
    /*
    val incCreditValid = Output(Bool())
    val incCreditChannel = Output(UInt(3.W))
    val incCreditAmount = Output(UInt(5.W))

    val incAccCreditValid = Output(Bool())
    val incAccCreditChannel = Output(UInt(3.W))
    val incAccCreditAmount = Output(UInt(5.W))

    val credits = Input(Vec(6, UInt(16.W)))
    val accCredits = Input(Vec(6, UInt(16.W)))
    val error = Input(Bool())
    */

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
  val rxState = RegInit(rxIdle)
  val rxComplete = RegInit(true.B)

  // Packet reception registers
  val rxPacketVec = Reg(Vec(68, UInt(64.W)))
  val rxPacketVecSize = RegInit(0.U(8.W))
  val rxCount = RegInit(0.U(8.W))
  val rxPacketEtherType = RegInit(0.U(16.W))

  val nextRxPacketVec = Reg(Vec(68, UInt(64.W)))
  val nextRxPacketVecSize = RegInit(0.U(8.W))

  val rxQueue = Module(new Queue(new Bundle {
    val rxPacketVec = Vec(68, UInt(64.W))
    val rxPacketVecSize = UInt(8.W)
  }, 8))

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

  /*
  io.incCreditValid := false.B
  io.incCreditChannel := 0.U
  io.incCreditAmount := 0.U

  io.incAccCreditValid := false.B
  io.incAccCreditChannel := 0.U
  io.incAccCreditAmount := 0.U
  */

  io.slideValid := false.B
  io.slideSeqNumAck := 0.U

  io.retransmitSeqNum := 0.U
  io.retransmitValid := false.B

  io.ackSeqNum := 0.U
  io.ackType := 0.U
  io.ackReady := false.B
  io.ackAckonly := false.B  

  // Simplified request type determination - avoid complex comparison logic
  def getReqType(rxSeq: UInt, nextRxSeq: UInt): UInt = {
    val diff = rxSeq - nextRxSeq
    // Simple comparison without complex arithmetic
    val reqType = WireDefault(0.U(2.W))
    when(diff === 0.U) {
      reqType := 0.U  // REQ_NORMAL
    }.elsewhen(diff === 0x3FFFFF.U) {  // -1 in 22-bit arithmetic
      reqType := 1.U  // REQ_DUPLICATE
    }.otherwise {
      reqType := 2.U  // REQ_OOS
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
      // Simplified offset calculation - avoid PriorityEncoder
      val mask = TloePacGen.getMask(nextRxPacketVec, nextRxPacketVecSize)
      val offset = Wire(UInt(6.W))
      offset := 0.U
      
      // Simple bit scanning instead of PriorityEncoder
      when(mask(0)) { offset := 0.U }
      .elsewhen(mask(1)) { offset := 1.U }
      .elsewhen(mask(2)) { offset := 2.U }
      .elsewhen(mask(3)) { offset := 3.U }
      .elsewhen(mask(4)) { offset := 4.U }
      .elsewhen(mask(5)) { offset := 5.U }
      .elsewhen(mask(6)) { offset := 6.U }
      .elsewhen(mask(7)) { offset := 7.U }
      .elsewhen(mask(8)) { offset := 8.U }
      .elsewhen(mask(9)) { offset := 9.U }
      .elsewhen(mask(10)) { offset := 10.U }
      .elsewhen(mask(11)) { offset := 11.U }
      .elsewhen(mask(12)) { offset := 12.U }
      .elsewhen(mask(13)) { offset := 13.U }
      .elsewhen(mask(14)) { offset := 14.U }
      .elsewhen(mask(15)) { offset := 15.U }
      .elsewhen(mask(16)) { offset := 16.U }
      .elsewhen(mask(17)) { offset := 17.U }
      .elsewhen(mask(18)) { offset := 18.U }
      .elsewhen(mask(19)) { offset := 19.U }
      .elsewhen(mask(20)) { offset := 20.U }
      .elsewhen(mask(21)) { offset := 21.U }
      .elsewhen(mask(22)) { offset := 22.U }
      .elsewhen(mask(23)) { offset := 23.U }
      .elsewhen(mask(24)) { offset := 24.U }
      .elsewhen(mask(25)) { offset := 25.U }
      .elsewhen(mask(26)) { offset := 26.U }
      .elsewhen(mask(27)) { offset := 27.U }
      .elsewhen(mask(28)) { offset := 28.U }
      .elsewhen(mask(29)) { offset := 29.U }
      .elsewhen(mask(30)) { offset := 30.U }
      .elsewhen(mask(31)) { offset := 31.U }
      .elsewhen(mask(32)) { offset := 32.U }
      .elsewhen(mask(33)) { offset := 33.U }
      .elsewhen(mask(34)) { offset := 34.U }
      .elsewhen(mask(35)) { offset := 35.U }
      .elsewhen(mask(36)) { offset := 36.U }
      .elsewhen(mask(37)) { offset := 37.U }
      .elsewhen(mask(38)) { offset := 38.U }
      .elsewhen(mask(39)) { offset := 39.U }
      .elsewhen(mask(40)) { offset := 40.U }
      .elsewhen(mask(41)) { offset := 41.U }
      .elsewhen(mask(42)) { offset := 42.U }
      .elsewhen(mask(43)) { offset := 43.U }
      .elsewhen(mask(44)) { offset := 44.U }
      .elsewhen(mask(45)) { offset := 45.U }
      .elsewhen(mask(46)) { offset := 46.U }
      .elsewhen(mask(47)) { offset := 47.U }
      .elsewhen(mask(48)) { offset := 48.U }
      .elsewhen(mask(49)) { offset := 49.U }
      .elsewhen(mask(50)) { offset := 50.U }
      .elsewhen(mask(51)) { offset := 51.U }
      .elsewhen(mask(52)) { offset := 52.U }
      .elsewhen(mask(53)) { offset := 53.U }
      .elsewhen(mask(54)) { offset := 54.U }
      .elsewhen(mask(55)) { offset := 55.U }
      .elsewhen(mask(56)) { offset := 56.U }
      .elsewhen(mask(57)) { offset := 57.U }
      .elsewhen(mask(58)) { offset := 58.U }
      .elsewhen(mask(59)) { offset := 59.U }
      .elsewhen(mask(60)) { offset := 60.U }
      .elsewhen(mask(61)) { offset := 61.U }
      .elsewhen(mask(62)) { offset := 62.U }
      .elsewhen(mask(63)) { offset := 63.U }

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

      rxFrameMask := mask
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

      when(reqType === 0.U) {
        rxState := rxFrameNormal
      }.elsewhen(reqType === 1.U) {
        rxState := rxFrameDup
      }.otherwise {
        rxState := rxFrameOOS
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

        io.rxPacketVec := nextRxPacketVec
        io.rxPacketVecSize := nextRxPacketVecSize
        io.doTilelinkHandler := true.B

        // Flow Control : decrease credit based on message type
        val rxRequiredFlits = TlMsgFlits.getFlitsCnt(tlHeader.chan, tlHeader.opcode, tlHeader.size)

        io.ackSeqNum := tloeHeader.seqNumAck
        io.ackType := TLOE_ACK
        io.ackReady := true.B

        rxState := rxHandleCredit
      }
    }

    is(rxHandleCredit) {
      /*
      io.incCreditChannel := tloeHeader.chan
      io.incCreditAmount := (1.U << tloeHeader.credit)
      io.incCreditValid := true.B
      */

      when(rxFrameMask === 0.U) {
        rxState := rxDone
      }.otherwise {
        rxState := rxHandleAccCredit
      }
    }

    is(rxFrameDup) {
      io.ackSeqNum := tloeHeader.seqNumAck
      io.ackType := TLOE_ACK
      io.ackReady := true.B

      rxState := rxDone
    }

    is(rxFrameOOS) {
      io.ackSeqNum := TLOESeqManager.getPrevSeq(io.nextRxSeq)
      io.ackType := TLOE_NAK
      io.ackReady := true.B

      rxState := rxDone
    }

    is(rxHandleAccCredit) {
      /*
      val rxRequiredFlits = TlMsgFlits.getFlitsCnt(tlHeader.chan, tlHeader.opcode, tlHeader.size)
      io.incAccCreditChannel := tlHeader.chan
      io.incAccCreditAmount := rxRequiredFlits
      io.incAccCreditValid := true.B
      */

      rxState := rxDone
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

  //////////////////////////////////////////////////////////////////
  // RX - Receive Path for AXI-Stream Data

  // If io.rxvalid signal is low, indicating no incoming data, reset rxcount
  when(!io.rxvalid) {
    rxCount := 0.U
  }

  val rxReceiveDone = RegInit(false.B)

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
      }
    }

    rxReceiveDone := false.B
  }
} 
