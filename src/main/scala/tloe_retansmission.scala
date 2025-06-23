package omnixtend

import chisel3._
import chisel3.util._

object RetransmissionConstants {
  val SRC_MAC = "h123456789ABC".U
  val DEST_MAC = "h001232FFFFFA".U
  val ETHER_TYPE = "hAAAA".U
}

class RetransmitBufferElement extends Bundle {
  //val tloeFrame = new TloePacket
  val tloeFrame = UInt(896.W)
  val seqNum = UInt(22.W)
  val vecSize = UInt(5.W)
  val state = UInt(2.W)
  val sendTime = UInt(64.W)
}

class Retransmission extends Module {
  import RetransmissionConstants._

  val io = IO(new Bundle {
    // Write interface - Transceiver writes packets to retransmit buffer
    val write = Input(new RetransmitBufferElement)
    val writeValid = Input(Bool())
    val writeReady = Output(Bool())  // Added writeReady output
    
    // Read interface - Retransmission reads packets from buffer
    val read = Input(UInt(896.W))
    
    // Control interface
    val clear = Input(Bool())
    val isEmpty = Output(Bool())
    val isFull = Output(Bool())
    val count = Output(UInt(5.W))
    
    // Retransmission interface
    val retransmitSeqNum = Input(UInt(22.W))
    val retransmitValid = Input(Bool())
    //val retransmitData = Input(UInt(896.W))
    val retransmitDone = Output(Bool())
    
    // Window slide interface
    val slideSeqNumAck = Input(UInt(22.W))
    val slideValid = Input(Bool())
    val slideDone = Output(Bool())

    // Ethernet interface
    val txdata = Output(UInt(64.W))
    val txvalid = Output(Bool())
    val txlast = Output(Bool())
    val txkeep = Output(UInt(8.W))
    //val txready = Input(Bool())

    val isRetransmit = Output(Bool())

    val currTime = Input(UInt(64.W))
  })

  val isRetransmit = dontTouch(RegInit(false.B))
  io.isRetransmit := isRetransmit

  val currTime = dontTouch(RegInit(0.U(64.W)))
  currTime := io.currTime

  // Buffer size
  val size = 16

  // Main buffer using Queue
  val retransmitBuffer = Module(new Queue(new RetransmitBufferElement, size))

  // Debug
  //val retransmitData = dontTouch(RegInit(0.U(896.W)))
  val retransmitValid = dontTouch(RegInit(false.B))
  val retransmitFirstElement = dontTouch(RegInit(0.U(896.W)))
  val retransmitCount = dontTouch(RegInit(0.U(5.W)))
  val retransmitSeqNum = dontTouch(RegInit(0.U(22.W)))
  val retransmitSendTime = dontTouch(RegInit(0.U(64.W)))
  
  // Initialize RetransmitBufferElement Wire
  val initElement = Wire(new RetransmitBufferElement)
  initElement.tloeFrame := 0.U(896.W)
  initElement.seqNum := 0.U(22.W)
  initElement.vecSize := 0.U(5.W)
  initElement.state := 0.U(2.W)
  initElement.sendTime := 0.U(64.W)
  
  val retransmitCurr = dontTouch(RegInit(initElement))

  //retransmitData := io.write.tloeFrame
  retransmitValid := io.writeValid
  retransmitFirstElement := retransmitBuffer.io.deq.bits.tloeFrame
  retransmitCount := retransmitBuffer.io.count
  retransmitSeqNum := retransmitBuffer.io.deq.bits.seqNum
  retransmitCurr := retransmitBuffer.io.deq.bits
  retransmitSendTime := retransmitBuffer.io.deq.bits.sendTime

  // Initialize Queue signals
  retransmitBuffer.io.enq.valid := false.B
  retransmitBuffer.io.enq.bits := initElement
  retransmitBuffer.io.deq.ready := false.B
  
  // Status signals
  io.isEmpty := retransmitBuffer.io.count === 0.U
  io.isFull := retransmitBuffer.io.count === size.U
  io.count := retransmitBuffer.io.count
  io.retransmitDone := false.B
  io.slideDone := false.B

  // Ethernet interface
  io.txdata := 0.U
  io.txvalid := false.B
  io.txlast := false.B
  io.txkeep := 0.U

  val modeRetransmit = dontTouch(RegInit(false.B))
  val modeSlideWindow = dontTouch(RegInit(false.B))
  val retransmitBufferEnqReady = dontTouch(RegInit(false.B))
  retransmitBufferEnqReady := retransmitBuffer.io.enq.ready

  //val retransmitFSize = dontTouch(RegInit(0.U(5.W)))
  //retransmitFSize := retransmitElement.fSize;

  //////////////////////////////////////////////////////////////////
  // Enqueue to retransmit buffer
  val retransmitBufferLastSeqNum = dontTouch(RegInit(0.U(22.W)))

  // Write logic
  when(io.writeValid) {
    retransmitBuffer.io.enq.valid := true.B
    retransmitBuffer.io.enq.bits := io.write
    retransmitBufferLastSeqNum := io.write.seqNum
    io.writeReady := retransmitBuffer.io.enq.ready
  }.otherwise {
    retransmitBuffer.io.enq.valid := false.B
    io.writeReady := false.B
  }

  // Packet generation and transmission
  val retransmitTxPacketVec = RegInit(VecInit(Seq.fill(14)(0.U(64.W))))
  val retransmitTxPacketVecSize = RegInit(0.U(4.W))
  val retransmitSendPacket = RegInit(false.B)
  val retransmitTxComplete = RegInit(false.B)
  val retransmitTxIndex = RegInit(0.U(4.W))

  val retransmitElement = RegInit(initElement)

  val retransmitSeqNumAck = dontTouch(RegInit(0.U(22.W)))
  val retransmitTCnt = dontTouch(RegInit(0.U(5.W)))
  val retransmitTDone = dontTouch(RegInit(false.B))

  retransmitTDone := io.retransmitDone

  val retransmitServeNAK = dontTouch(RegInit(false.B))
  val retransmitServeTimeout = dontTouch(RegInit(false.B))

  //////////////////////////////////////////////////////////////////
  // Timeout Check and retransmission
  val timeoutCheck = dontTouch(RegInit(false.B))
  val timeoutRetransmit = dontTouch(RegInit(false.B))
  val lastTimeoutCheck = dontTouch(RegInit(0.U(64.W)))
  val timeoutDelta = dontTouch(RegInit(0.U(64.W)))

  // Timeout check logic using Timer's isTimeout function
  when(retransmitBuffer.io.count =/= 0.U) {
    // Check every 10 seconds (1 billion cycles at 100MHz)
    timeoutDelta := currTime - lastTimeoutCheck
    when(timeoutDelta >= 1000000000.U) {
      timeoutCheck := true.B
      lastTimeoutCheck := currTime
      when(Timer.isTimeout(currTime, retransmitSendTime)) {
        timeoutRetransmit := true.B
        timeoutCheck := false.B
      }
    }.otherwise {
      timeoutRetransmit := false.B
    }
  }.otherwise {
    timeoutCheck := false.B
    timeoutRetransmit := false.B
  }

  val timeoutCnt = dontTouch(RegInit(0.U(10.W)))

  /*
  when(timeoutRetransmit) {
    when (!retransmitServeTimeout) {
      timeoutCnt := timeoutCnt + 1.U
      retransmitSeqNumAck := retransmitCurr.seqNum - 1.U // TODO modify seqnum
      modeRetransmit := true.B
      isRetransmit := true.B
      when (retransmitState === 0.U) {
        retransmitState := 1.U  // Start with dequeue
      }
      timeoutRetransmit := false.B
    }.otherwise {
      timeoutRetransmit := false.B
    }
  }
    */
  /*
  // Retransmit on timeout
  when(timeoutRetransmit && !modeRetransmit) {
    timeoutCnt := timeoutCnt + 1.U
    retransmitSeqNumAck := retransmitCurr.seqNum - 1.U // TODO modify seqnum
    modeRetransmit := true.B
    isRetransmit := true.B
    retransmitState := 1.U  // Start with dequeue
    timeoutRetransmit := false.B
  }.otherwise {
    timeoutRetransmit := false.B
  }
  */

  //////////////////////////////////////////////////////////////////
  // Retransmit
  // Debug
  val retransmitSlideWindow = dontTouch(RegInit(false.B))
  val retransmitSlideWindowNum = dontTouch(RegInit(0.U(22.W)))
  val retransmitRetransmit = dontTouch(RegInit(false.B))
  val retransmitRetransmitNum = dontTouch(RegInit(0.U(22.W)))

  retransmitSlideWindow := io.slideValid
  retransmitSlideWindowNum := io.slideSeqNumAck
  retransmitRetransmit := io.retransmitValid
  retransmitRetransmitNum := io.retransmitSeqNum

  val rtIdle :: rtDequeue :: rtSend :: rtEnqueue :: rtDone :: Nil = Enum(5)
  val retransmitState = dontTouch(RegInit(rtIdle))

  when (io.retransmitValid || timeoutRetransmit) {
    when (!retransmitServeNAK) {
      retransmitSeqNumAck := io.retransmitSeqNum
      modeRetransmit := true.B
      isRetransmit := true.B
      when (retransmitState === rtIdle) {
        retransmitState := rtDequeue  // Start with dequeue
      }
    }.otherwise {
      io.retransmitDone := true.B
    }
  }

  when(modeRetransmit) {
    switch(retransmitState) {
      is(rtIdle) {  // Idle state
        retransmitBuffer.io.deq.ready := false.B
        retransmitBuffer.io.enq.valid := false.B
      }
      is(rtDequeue) {  // Dequeue state
        when(retransmitBuffer.io.deq.valid) {
          retransmitElement := retransmitBuffer.io.deq.bits
          retransmitBuffer.io.deq.ready := true.B
          retransmitTCnt := retransmitTCnt + 1.U
          retransmitState := rtSend  // Move to send state
        }.otherwise {
          // Buffer is empty
          modeRetransmit := false.B
          isRetransmit := false.B
          retransmitBuffer.io.deq.ready := false.B
          //retransmitState := 0.U
          io.retransmitDone := true.B
        }
      }
      is(rtSend) {  // Send state
        retransmitBuffer.io.deq.ready := false.B
        sendRetransmitFrame(retransmitElement.tloeFrame, retransmitElement.vecSize)
        retransmitTxComplete := true.B
        retransmitState := rtEnqueue
      }
      is(rtEnqueue) {  // Enqueue state
        when(!retransmitTxComplete) {
          retransmitBuffer.io.enq.valid := true.B
          retransmitBuffer.io.enq.bits := retransmitElement
          retransmitBuffer.io.enq.bits.sendTime := currTime

          when(retransmitBuffer.io.enq.ready) {
            retransmitState := rtDone
          }
        }
      }
      is(rtDone) {
        //when (retransmitBufferLastSeqNum === retransmitElement.seqNum) {
        when (TLOESeqManager.seqNumCompare(retransmitBufferLastSeqNum, retransmitElement.seqNum) === 0.S) {
          modeRetransmit := false.B
          isRetransmit := false.B
          //retransmitState := 0.U
          io.retransmitDone := true.B
          timeoutRetransmit := false.B
        }.otherwise {
          retransmitState := rtDequeue
        }
      }
   }
  }.otherwise {
    retransmitState := rtIdle
    io.retransmitDone := false.B
  }
 
  val retransmitSlideWindowElement = dontTouch(RegInit(0.U(896.W)))
  val retransmitSlideWindowElementSeqNum = dontTouch(RegInit(0.U(22.W)))
  val retransmitSlideSeqNumAck = dontTouch(RegInit(0.U(22.W) ))
  val retransmitSlideDone = dontTouch(RegInit(false.B))
  val retransmitSlideValid = dontTouch(RegInit(false.B))
  val retransitSlideTCnt = dontTouch(RegInit(0.U(5.W)))

  retransmitSlideValid := io.slideValid
  retransmitSlideDone := io.slideDone

  //////////////////////////////////////////////////////////////////
  // Slide Window logic
  when(io.slideValid) {
    retransmitSlideSeqNumAck := io.slideSeqNumAck
    modeSlideWindow := true.B
  }

  val swIdle :: swDequeue :: swCompare :: swSlide :: swDone :: Nil = Enum(5)
  val slideWindowState = dontTouch(RegInit(swIdle))

  when (modeSlideWindow) {
    switch(slideWindowState) {
      is(swIdle) {
        retransmitBuffer.io.deq.ready := false.B
        slideWindowState := swDequeue
      }
      is(swDequeue) {
        when(retransmitBuffer.io.deq.valid) {
          val element = retransmitBuffer.io.deq.bits
          retransmitSlideWindowElement := element.tloeFrame
          retransmitSlideWindowElementSeqNum := element.tloeFrame(773, 752)

          slideWindowState := swCompare
        }.otherwise {
          slideWindowState := swDone
        }
      }
      is(swCompare) {
        //when(retransmitSlideWindowElementSeqNum <= retransmitSlideSeqNumAck || io.isEmpty) {
        when(TLOESeqManager.seqNumCompare(retransmitSlideWindowElementSeqNum, retransmitSlideSeqNumAck) <= 0.S) {
          slideWindowState := swSlide
        }.otherwise {
          slideWindowState := swDone
        }
      }
      is(swSlide) {
        retransmitBuffer.io.deq.ready := true.B
        slideWindowState := swDequeue
      }
      is(swDone) {
        modeSlideWindow := false.B
        io.slideDone := true.B
        slideWindowState := swIdle
      }
    }
  }

  when(io.clear) {
    retransmitBuffer.reset := true.B
  }

  //////////////////////////////////////////////////////////////////
  // Packet generation and transmission
  def sendRetransmitFrame(tloeFrame: UInt, vecSize: UInt) = {
    // In the next clock cycle, split into 64-bit chunks for transmission
    retransmitTxPacketVec := VecInit(Seq.tabulate(14) { i =>
      val high = 896 - (64 * i) - 1
      val low = math.max(896 - 64 * (i + 1), 0)
      tloeFrame(high, low)
    })

    //Opcode에 따라 다시 확인해야하는게 아닌지?
    retransmitTxPacketVecSize := vecSize // TODO
    retransmitSendPacket := true.B
    retransmitTxIndex := 0.U
  }

  // Packet transmission state machine
  when(retransmitSendPacket) {
    when(retransmitTxIndex < retransmitTxPacketVecSize) {
      io.txdata := TloePacGen.toBigEndian(retransmitTxPacketVec(retransmitTxIndex))
      io.txvalid := true.B

      // Handle last packet differently
      when(retransmitTxIndex === (retransmitTxPacketVecSize - 1.U)) {
        io.txlast := true.B
        io.txkeep := "h3F".U
      }.otherwise {
        io.txlast := false.B
        io.txkeep := "hFF".U
      }

      retransmitTxIndex := retransmitTxIndex + 1.U
    }.elsewhen(retransmitTxIndex >= retransmitTxPacketVecSize) {
      // Reset all signals explicitly
      io.txdata := 0.U
      io.txvalid := false.B
      io.txlast := false.B
      io.txkeep := 0.U

      retransmitTxIndex := 0.U
      retransmitSendPacket := false.B
      retransmitTxComplete := false.B
    }
  }
} 
