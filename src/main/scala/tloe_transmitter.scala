package omnixtend

import chisel3._
import chisel3.util._

class TLOETransmitter extends Module {
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

    // Sequence Management
    val incTxSeq = Output(Bool())
    val incRxSeq = Output(Bool())
    val updateAckSeq = Output(Bool())
    val newAckSeq = Output(UInt(22.W))

    val nextTxSeq = Input(UInt(22.W))
    val nextRxSeq = Input(UInt(22.W))
    val ackdSeq = Input(UInt(22.W))

    // Flow Control
    val decCreditValid = Output(Bool())
    val decCreditChannel = Output(UInt(3.W))
    val decCreditAmount = Output(UInt(16.W))

    val decAccCreditValid = Output(Bool())
    val decAccCreditChannel = Output(UInt(3.W))
    val decAccCreditAmount = Output(UInt(16.W))

    val credits = Input(Vec(6, UInt(16.W)))
    val accCredits = Input(Vec(6, UInt(16.W)))
    val maxCreditChannel = Input(UInt(3.W))
    val error = Input(Bool())
    val maxCredit = Input(UInt(16.W))

    // Retransmission
    val retransmitWrite = Output(new RetransmitBufferElement)
    val retransmitWriteValid = Output(Bool())
    val retransmitIsFull = Input(Bool())

    val isRetransmit = Input(Bool())

    // Timer
    val currTime = Input(UInt(64.W))

    //
    val ackSeqNum = Input(UInt(22.W))
    val ackType = Input(UInt(2.W))
    val ackReady = Input(Bool())    
    val ackAckonly = Input(Bool())
    val ackAckonlyDone = Output(Bool())

    val epConn = Input(Bool())

    val debug1 = Input(Bool())
    val debug2 = Input(Bool())
  })

  io.txdata := 0.U
  io.txvalid := false.B
  io.txlast := false.B
  io.txkeep := 0.U

  io.incTxSeq := false.B
  io.incRxSeq := false.B
  io.updateAckSeq := false.B
  io.newAckSeq := 0.U

  io.decCreditValid := false.B
  io.decCreditChannel := 0.U
  io.decCreditAmount := 0.U

  io.decAccCreditValid := false.B
  io.decAccCreditChannel := 0.U
  io.decAccCreditAmount := 0.U

  io.retransmitWrite.tloeFrame := 0.U(896.W)
  io.retransmitWrite.vecSize := 0.U(5.W)
  io.retransmitWrite.state := 0.U(2.W)
  io.retransmitWrite.sendTime := 0.U(64.W)
  io.retransmitWrite.seqNum := 0.U(22.W)
  io.retransmitWriteValid := false.B

  io.ackAckonlyDone := false.B

  val epConn = dontTouch(RegInit(false.B))
  epConn := io.epConn

  val txIdle :: txAckOnly :: txCheckFrame :: txCheckAck :: txCheckCredit :: txInitFrame :: txHandleCredit :: txHandleAccCredit :: txPrepareSend :: txSendPacket :: txEnqRetransmit :: txDone :: Nil = Enum(12)
  val txState = dontTouch(RegInit(txIdle))

  val aidle :: amakeFrame :: asendRequest :: adone :: Nil = Enum(4)
  val astate = RegInit(aidle)

  val tx_size = dontTouch(RegInit(0.U(3.W)))

  val nextOpcode = dontTouch(RegInit(0.U(3.W)))
  val nextParam = dontTouch(RegInit(0.U(4.W)))
  val nextSize = dontTouch(RegInit(0.U(4.W)))
  val nextSource = dontTouch(RegInit(0.U(26.W)))
  val nextAddr = dontTouch(RegInit(0.U(64.W)))
  val nextData = dontTouch(RegInit(0.U(512.W)))

  val isFrame = dontTouch(RegInit(false.B))
  val isACK = dontTouch(RegInit(false.B))
  val isCredit = dontTouch(RegInit(false.B))

  val maxAccChannel = dontTouch(RegInit(0.U(3.W)))
  val maxAccCredit = dontTouch(RegInit(0.U(16.W)))
  maxAccChannel := io.maxCreditChannel
  maxAccCredit := io.maxCredit

  // Register for storing read (rPacket) and write (wPacket) packets
  val txPacket = dontTouch(RegInit(0.U(896.W)))
  val txPacketSize = dontTouch(RegInit(0.U(5.W)))
  val txRequiredFlits = dontTouch(RegInit(0.U(8.W)))

  // TODO delete
  val nAckPacket = dontTouch(RegInit(0.U(576.W)))

  val txPacketVec = dontTouch(RegInit(VecInit(Seq.fill(14)(0.U(64.W)))))
  val txPacketVecSize = dontTouch(RegInit(0.U(5.W)))

  val sendPacket = dontTouch(RegInit(false.B))
  val txComplete = dontTouch(RegInit(true.B))
  val idx = dontTouch(RegInit(0.U(16.W)))

  // Registers for AXI-Stream transmission state
  val axi_txdata = dontTouch(RegInit(0.U(64.W)))
  val axi_txvalid = dontTouch(RegInit(false.B))
  val axi_txlast = dontTouch(RegInit(false.B))
  val axi_txkeep = dontTouch(RegInit(0.U(8.W)))  

  // Connect internal signals to external IO interface for AXI transmission
  io.txvalid := axi_txvalid
  io.txdata := axi_txdata
  io.txlast := axi_txlast
  io.txkeep := axi_txkeep

   // Single integrated queue for transmission data
  val txQueue = Module(new Queue(new Bundle {
    val opcode = UInt(3.W)
    val param = UInt(4.W)
    val size = UInt(4.W)
    val source = UInt(26.W)
    val addr = UInt(64.W)
    val data = UInt(512.W)
  }, 16))

  // Default queue port values
  txQueue.io.enq.valid := false.B
  txQueue.io.enq.bits := 0.U.asTypeOf(txQueue.io.enq.bits)
  txQueue.io.deq.ready := false.B


  // Debug
  val testReadAddr = RegInit(0x1000.U(64.W))
  when(epConn && io.debug1) {
    txQueue.io.enq.bits.addr := testReadAddr
    txQueue.io.enq.bits.opcode := 4.U
    txQueue.io.enq.bits.size := 6.U
    txQueue.io.enq.bits.data := 0.U
    txQueue.io.enq.valid := true.B

    testReadAddr := testReadAddr + 0x1000.U
  }

  val ackReadyFlag = dontTouch(RegInit(false.B))
  val ackTypeFlag = dontTouch(RegInit(0.U(2.W)))
  val ackSeqNumFlag = dontTouch(RegInit(0.U(22.W)))

  when(io.ackReady) {
    ackReadyFlag := io.ackReady
    ackTypeFlag := io.ackType
    ackSeqNumFlag := io.ackSeqNum
  }

  // Enqueue data into the queue when txValid is asserted
  when(io.txValid) {
    txQueue.io.enq.bits.opcode := io.txOpcode
    txQueue.io.enq.bits.param := io.txParam
    txQueue.io.enq.bits.size := io.txSize
    txQueue.io.enq.bits.source := io.txSource
    txQueue.io.enq.bits.addr := io.txAddr
    txQueue.io.enq.bits.data := io.txData
    txQueue.io.enq.valid := true.B
  }
  
  val creditADecCntDebug = dontTouch(RegInit(0.U(22.W)))
  val maxCreditChannelDebug = dontTouch(RegInit(0.U(3.W)))
  val maxCreditDebug = dontTouch(RegInit(0.U(16.W)))

  val txAccChannel = dontTouch(RegInit(0.U(3.W)))
  val txAccCredit = dontTouch(RegInit(0.U(5.W)))

  when(io.maxCreditChannel =/= 0.U) {
    val maxCreditChannelDDebug = io.maxCreditChannel

    maxCreditChannelDebug := io.maxCreditChannel
    maxCreditDebug := io.maxCredit  // Use the maxCredit output from FlowControl
  }

  switch(txState) {
    is(txIdle) {
      when(txComplete) {
        when(io.ackAckonly) {
          txState := txAckOnly
        }.otherwise {
          txState := txCheckFrame
        }
      }
    }

    // TODO 처리할 메시지가 있으며 ackonly 프레임을 보내야할까?
    is(txAckOnly) {
      astate := amakeFrame
      io.ackAckonlyDone := true.B
      txComplete := false.B

      txState := txDone
    }

    is(txCheckFrame) {
      when (!io.retransmitIsFull && txQueue.io.deq.valid) {
        nextOpcode := txQueue.io.deq.bits.opcode
        nextParam := txQueue.io.deq.bits.param
        nextSize := txQueue.io.deq.bits.size
        nextSource := txQueue.io.deq.bits.source
        nextAddr := txQueue.io.deq.bits.addr
        nextData := txQueue.io.deq.bits.data
        txQueue.io.deq.ready := true.B

        isFrame := true.B
      }
      txState := txCheckAck
    }
    is(txCheckAck) {
      when(ackReadyFlag) {
        isACK := true.B
      }
      txState := txCheckCredit
    }


    is(txCheckCredit) {
      when(maxAccChannel =/= 0.U) {
        when(maxAccCredit > 0.U) {
          isCredit := true.B

          txAccChannel := maxAccChannel
          txAccCredit := maxAccCredit
        }
      }
      txState := txInitFrame
    } 

    is(txInitFrame) {
      when(isFrame) {
        txPacket := OXPacket.initFrame(nextAddr, nextOpcode, nextData, io.nextTxSeq, TLOESeqManager.getPrevSeq(io.nextRxSeq), io.ackType, txAccChannel, txAccCredit, nextSize, nextParam, nextSource)
        txPacketSize := nextSize

        isFrame := false.B
        isACK := false.B
        txComplete := false.B

        ackReadyFlag := false.B
        ackTypeFlag := 0.U
        ackSeqNumFlag := 0.U

        txState := txHandleCredit
      }.elsewhen(isACK || isCredit) {
        txPacket := OXPacket.normalAck_896(io.nextTxSeq, TLOESeqManager.getPrevSeq(io.nextRxSeq), 1.U, txAccChannel, txAccCredit)
        txPacketSize := 0.U

        isFrame := false.B
        isACK := false.B
        txComplete := false.B

        ackReadyFlag := false.B
        ackTypeFlag := 0.U
        ackSeqNumFlag := 0.U

        txState := txHandleAccCredit
      }.otherwise {
        txState := txIdle
      }
    }

    is(txHandleCredit) {
      // Flow Control : decrease credit based on message type
      val decFlits = TlMsgFlits.getFlitsCnt(1.U, nextOpcode, nextSize)
      val hasEnoughCredit = io.credits(1.U) >= decFlits

      when(hasEnoughCredit) {
        io.decCreditValid := true.B
        io.decCreditChannel := 1.U  // Channel A
        io.decCreditAmount := decFlits 
        creditADecCntDebug := creditADecCntDebug + decFlits
        txState := txHandleAccCredit
      }.otherwise {
        io.decCreditValid := false.B
        io.decCreditChannel := 0.U
        io.decCreditAmount := 0.U
        txState := txHandleCredit  // Stay in the same state if not enough credit
      }
    }

    is(txHandleAccCredit) {
      //Flow Control
      //when(maxAccChannel =/= 0.U) {
      when(isCredit) {
        io.decAccCreditValid := true.B
        /*
        io.decAccCreditChannel := maxAccChannel
        io.decAccCreditAmount := (1.U << maxAccCredit)
        */
        io.decAccCreditChannel := txAccChannel
        io.decAccCreditAmount := (1.U << txAccCredit)

        isCredit := false.B
      }.otherwise {
        txAccChannel := 0.U
        txAccCredit := 0.U

        txState := txPrepareSend
      }
    }

    is(txPrepareSend) {
      // Prepare the read packet by dividing rPacket into 64-bit segmentsj and storing in txPacketVec
      txPacketVec := VecInit(Seq.tabulate(14) { i => txPacket(896 - (64 * i) - 1, 896 - 64 * (i + 1))
      })
      txPacketVecSize := Mux(nextOpcode === 4.U, 9.U, Mux(txPacketSize <= 5.U, 9.U, 14.U))

      // Increase sequence number
      io.incTxSeq := true.B  // Set incTxSeq when incrementing TX sequence

      sendPacket := true.B // Indicate that packet is ready to send

      txState := txEnqRetransmit
    }

    is(txEnqRetransmit) {
      // Enqueue the packet to the retransmit buffer
      io.retransmitWrite.tloeFrame := txPacket
      io.retransmitWrite.seqNum := io.nextTxSeq
      io.retransmitWrite.vecSize := txPacketVecSize
      io.retransmitWrite.state := 0.U(2.W)  // Initial state
      io.retransmitWrite.sendTime := io.currTime  // Use global timer 

      io.retransmitWriteValid := true.B  // Set valid signal when writing

      txState := txDone
    }

    is(txDone) {
      when(txComplete) {
        txState := txIdle
        txComplete := true.B
      }
    }
  }

  //Debug
  var isRetransmit = dontTouch(RegInit(false.B))
  isRetransmit := io.isRetransmit

  //////////////////////////////////////////////////////////////////
  // Packet Sending Logic
  // TODO need to define as function

  // State machine for sending packets via AXI-Stream interface
  when(sendPacket && !io.isRetransmit) {
    when(idx < txPacketVecSize) {
      // Store current packet data in axi_txdata
      axi_txdata := TloePacGen.toBigEndian(txPacketVec(idx))
      axi_txvalid := true.B // Set valid signal
      idx := idx + 1.U // Move to next packet

      // Check if this is the last packet
      when(idx === (txPacketVecSize - 1.U)) {
        axi_txlast := true.B // Set last packet flag
        axi_txkeep := 0x3f.U // Last packet flag
        idx := 20.U // Reset index
      }.otherwise {
        axi_txlast := false.B
        axi_txkeep := 0xff.U
      }
    }.otherwise {
      // Reset values after packet sending
      axi_txdata := 0.U
      axi_txvalid := false.B
      axi_txlast := false.B
      axi_txkeep := 0.U

      idx := 0.U
      sendPacket := false.B
      txComplete := true.B
    }
  }


  //////////////////////////////////////////////////////////////////
  // SEND - Packet Send (Ack Only)

  // Send an ACKONLY frame

  switch(astate) {
    // Create normal frame
    is(amakeFrame) {
      nAckPacket := OXPacket.ackonly(io.nextTxSeq, io.nextRxSeq - 1.U, 1.U, 0.U, 0.U)
      astate := asendRequest
    }

    // Send ank only frame
    is(asendRequest) {
      // Prepare the read packet by dividing rPacket into 64-bit segments and storing in txPacketVec
      txPacketVec := VecInit(Seq.tabulate(9) { i =>
        nAckPacket(576 - (64 * i) - 1, 576 - 64 * (i + 1))
      } ++ Seq.fill(5)(0.U(64.W)))

      txPacketVecSize := 9.U // Set the size of the packet vector

      sendPacket := true.B // Indicate that packet is ready to send
      txComplete := false.B // Reset transmission complete flag

      astate := adone // Move to wait for credit acknowledgment
    }

    is(adone) {
      astate := aidle
    }
  }
} 
