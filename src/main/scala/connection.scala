package omnixtend

import chisel3._
import chisel3.util._

// Constants for OmniXtend protocol
object TloeConnectionConstants {
  val CHANNEL_NUM = 6
  val CREDIT_DEFAULT = 9  //TODO
  //val CREDIT_DEFAULT = 20
  val CONN_PACKET_SIZE = 72  // Size in bytes
  val CONN_RESEND_TIME = 5000000L  // 5 seconds in microseconds
  
  // Message types
  val TYPE_NORMAL = 0.U(4.W)
  val TYPE_ACKONLY = 1.U(4.W)
  val TYPE_OPEN_CONNECTION = 2.U(4.W)
  val TYPE_CLOSE_CONNECTION = 3.U(4.W)
  
  // Channels
  val CHANNEL_A = 1.U(3.W)
  val CHANNEL_B = 2.U(3.W)
  val CHANNEL_C = 3.U(3.W)
  val CHANNEL_D = 4.U(3.W)
  val CHANNEL_E = 5.U(3.W)

  val SRC_MAC = "h123456789ABC".U
  val DEST_MAC = "h001232FFFFFA".U
  val ETHER_TYPE = "hAAAA".U
}

/**
 * TLOEConnection handles the connection management for OmniXtend protocol.
 * It manages connection establishment, maintenance, and termination.
 */
class TloeConnection extends Module {
  import TloeConnectionConstants._
  
  val io = IO(new Bundle {
    // Control signals
    val oxOpen  = Input(Bool())
    val oxClose = Input(Bool())
    val reset = Input(Bool())

    // Ethernet interface
    val txdata   = Output(UInt(64.W))
    val txvalid  = Output(Bool())
    val txlast   = Output(Bool())
    val txkeep   = Output(UInt(8.W))
    val txready  = Input(Bool())
    val rxdata   = Input(UInt(64.W))
    val rxvalid  = Input(Bool())
    val rxlast   = Input(Bool())

    // Connection state
    val isConn = Output(Bool())
    
    // Flow control interface
    val incCreditValid = Output(Bool())
    val incCreditChannel = Output(UInt(3.W))
    val incCreditAmount = Output(UInt(5.W))

    // Sequence number interface
    val nextTxSeq = Input(UInt(22.W))
    val nextRxSeq = Input(UInt(22.W))
    val ackdSeq = Input(UInt(22.W))
    val incTxSeq = Output(Bool())
    val incRxSeq = Output(Bool())
    val updateAckSeq = Output(Bool())
    val newAckSeq = Output(UInt(22.W))
  })

  val rxdata_conn = dontTouch(RegInit(0.U(64.W)))
  val rxvalid_conn = dontTouch(RegInit(false.B))
  val rxlast_conn = dontTouch(RegInit(false.B))
  rxdata_conn := io.rxdata
  rxvalid_conn := io.rxvalid
  rxlast_conn := io.rxlast

  // Initialize default output values
  io.txdata := 0.U
  io.txvalid := false.B
  io.txlast := false.B
  io.txkeep := 0.U

  io.isConn := false.B

  io.incTxSeq := false.B
  io.incRxSeq := false.B
  io.updateAckSeq := false.B
  io.newAckSeq := 0.U

  // Initialize flow control outputs
  io.incCreditValid := false.B
  io.incCreditChannel := 0.U
  io.incCreditAmount := 0.U

  val isConn = dontTouch(RegInit(false.B))
  io.isConn := isConn

  // Packet generation and transmission
  val connTxPacketVec = RegInit(VecInit(Seq.fill(14)(0.U(64.W))))
  val connTxPacketVecSize = RegInit(0.U(4.W))
  val connSendPacket = RegInit(false.B)
  val connTxComplete = RegInit(false.B)
  val connTxIndex = RegInit(0.U(4.W))

  val connRxPacketVec = RegInit(VecInit(Seq.fill(14)(0.U(64.W))))
  val connRxPacketVecSize = RegInit(0.U(4.W))
  val connRxCount = RegInit(0.U(8.W))

  val connRxSlave = dontTouch(RegInit(false.B))
  val connRxSlaveReceived = dontTouch(RegInit(false.B))
  val connRxMasterReceived = dontTouch(RegInit(false.B))

  // Connection state
  val openIdle :: openServing :: openWaiting :: Nil = Enum(3)
  val openState = RegInit(openIdle)

  val closeIdle :: closeServing :: closeWaiting :: Nil = Enum(3)
  val closeState = RegInit(closeIdle)

  val openTx = dontTouch(RegInit(false.B))
  val openRx = dontTouch(RegInit(true.B)) 
  val openRxReceived = dontTouch(RegInit(false.B))
  val openRxServed = dontTouch(RegInit(false.B))
  val allChannelsResponded = dontTouch(RegInit(false.B))
  val connCompleted = dontTouch(RegInit(false.B))

  val openRxSlaveServed = dontTouch(RegInit(false.B))

  // Add timer and channel tracking registers
  val isTimer = dontTouch(RegInit(false.B))
  val timerCount = dontTouch(RegInit(0.U(32.W)))
  val channelIndex = dontTouch(RegInit(1.U(3.W)))
  val channelResponses = dontTouch(RegInit(VecInit(Seq.fill(6)(false.B))))

  val isSlaveTimer = dontTouch(RegInit(false.B))
  val slaveTimerCount = dontTouch(RegInit(0.U(32.W)))

  // Add registers for channel and credit information
  val channelInfo = RegInit(VecInit(Seq.fill(6)(0.U(3.W))))
  val creditInfo = RegInit(VecInit(Seq.fill(6)(0.U(5.W))))

  //////////////////////////////////////////////////////////////////
  // Open connection (master mode)
  // Add connection state tracking
  val connInProgress = RegInit(false.B)

  when (io.oxOpen) {
    when (isConn === false.B) {
      openState := openServing
      isTimer := true.B
      timerCount := 0.U
      channelIndex := 1.U
      channelResponses := VecInit(Seq.fill(6)(false.B))
      channelInfo := VecInit(Seq.fill(6)(0.U(3.W)))
      creditInfo := VecInit(Seq.fill(6)(0.U(5.W)))
      openTx := true.B
      //openRx := true.B
      connInProgress := true.B
      connTxComplete := false.B
    }
  }

  // Only retry when connection is in progress and not all channels have responded
  when(isTimer) {
    when(connInProgress && !allChannelsResponded && timerCount >= CONN_RESEND_TIME.U) {
      timerCount := 0.U
      openTx := true.B    
    }.otherwise {
      timerCount := timerCount + 1.U  
    }
  }

  // Helper function to get previous sequence number
  def getPrevSeqNum(seqNum: UInt): UInt = {
    Mux(seqNum === 0.U, TLOESeqManagetConstant.MAX_SEQ_NUM.U, seqNum - 1.U)
  }

  // TODO Where channelIndex is set?
  when(openTx && !connTxComplete) {
    when(channelIndex < CHANNEL_NUM.U) {
      // Send connection message - first channel with TYPE_OPEN_CONNECTION, others with TYPE_NORMAL
      val msgType = Mux(channelIndex === CHANNEL_A, TYPE_OPEN_CONNECTION, TYPE_NORMAL)

      connTxComplete := true.B
      send_frame(msgType, io.nextTxSeq, getPrevSeqNum(io.nextRxSeq), channelIndex, CREDIT_DEFAULT.U)

      // Request sequence number increment
      io.incTxSeq := true.B
      channelIndex := channelIndex + 1.U
    }.elsewhen(channelIndex === CHANNEL_NUM.U) {
      openTx := false.B
    }
  }

  when(openRx) {
     // When io.rxvalid is high, data is being received
    when(io.rxvalid) {
      connRxCount := connRxCount + 1.U

      // Store incoming data at the current rxcount index in rPacketVec vector
      connRxPacketVec(connRxCount) := io.rxdata

      // Check if io.rxlast is high, signaling the end of the packet
      when(io.rxlast) {
        connRxPacketVecSize := connRxCount + 1.U

        //TODO: check ethertype == Omnixtend
        openRxReceived := true.B

       // Reset rxcount to prepare for the next incoming packet
        connRxCount := 0.U
      }
    }
  }

    val connRxChannel = dontTouch(RegInit(0.U(3.W)))
    val connRxCredit = dontTouch(RegInit(0.U(5.W)))
    val connRxType = dontTouch(RegInit(0.U(4.W)))
    val connRxSeqNum = dontTouch(RegInit(0.U(22.W)))

  when(openRxReceived) {
    connRxChannel := TloePacGen.getChan(connRxPacketVec)
    connRxCredit := TloePacGen.getCredit(connRxPacketVec)
    connRxType := TloePacGen.getMsgType(connRxPacketVec)
    connRxSeqNum := TloePacGen.getSeqNum(connRxPacketVec)

    openRxReceived := false.B
    openRxServed := true.B  
  }

  when(openRxServed) {
    when(connRxType === TYPE_OPEN_CONNECTION) {
      connRxSlaveReceived := true.B
      openRxServed := false.B
    }.otherwise {
      connRxMasterReceived := true.B
      openRxServed := false.B
    }
  }

  when(connRxMasterReceived) {
    when(connRxType === TYPE_ACKONLY) {
      //TODO: check if ackdSeq is correct
      io.updateAckSeq := true.B
      io.newAckSeq := connRxSeqNum

      connCompleted := true.B
    }
          
    // Update channel response status only if not already processed
    when(connRxChannel =/= 0.U && !channelResponses(connRxChannel)) {
      channelResponses(connRxChannel) := true.B

      // Request sequence number increment if received sequence matches expected
      when(connRxSeqNum === io.nextRxSeq) {
        io.incRxSeq := true.B     // Request increment from sequence manager
      }

      // Send credit information to flow control only for new channel responses
      when(connRxChannel =/= 0.U) {
        io.incCreditValid := true.B
        io.incCreditChannel := connRxChannel
        io.incCreditAmount := (1.U << connRxCredit)
      }
    }
    openRxServed := false.B
  }

  val isSendAck = dontTouch(RegInit(false.B))
  // Check if all channels responded
  //when(channelResponses.reduce(_ && _)) {
  // Check if channels A, C, and E have responded
  when(!isSendAck && channelResponses(CHANNEL_A) && channelResponses(CHANNEL_C) && channelResponses(CHANNEL_E)) {
    connTxComplete := true.B
    send_frame(TYPE_NORMAL, io.nextTxSeq, getPrevSeqNum(io.nextRxSeq), 0.U, 0.U)
    // Request sequence number increment
    io.incTxSeq := true.B
    isSendAck := true.B
  }

  // Reset flags only after packet transmission is complete
  when(connCompleted && !connTxComplete) {
    isTimer := false.B
    openTx := false.B
    openRx := false.B
    isConn := true.B
    openState := openIdle
    connInProgress := false.B
  }

  //////////////////////////////////////////////////////////////////
  // Close connection
  when (io.oxClose) {
    // TODO TBD
  }

  //////////////////////////////////////////////////////////////////
  // Open connection (slave mode)
  val openTxSlave = dontTouch(RegInit(false.B))
  val openRxSlave = dontTouch(RegInit(false.B))
  val openRxSlaveNormal = dontTouch(RegInit(false.B)) 
  val openRxSlaveComplete = dontTouch(RegInit(false.B))
  val openTxSlaveInit = dontTouch(RegInit(false.B))

  when(connRxSlaveReceived) {
    when(connInProgress === false.B) {
      isSlaveTimer := true.B
      slaveTimerCount := 0.U
      channelIndex := 1.U
      channelResponses := VecInit(Seq.fill(6)(false.B))

      openRxSlave := true.B  
      connInProgress := true.B
      connTxComplete := false.B
      connRxSlaveReceived := false.B
    }.otherwise {
      openRxSlave := true.B  
      connRxSlaveReceived := false.B
    }
  }

  when(isSlaveTimer) {
    // Send initial open connection message
    when(!openTxSlaveInit) {
      openTxSlave := true.B
      openTxSlaveInit := true.B
    }

    when(!allChannelsResponded && slaveTimerCount >= CONN_RESEND_TIME.U) {
      // Send open connection message if not all channels responded & timer has expired
      slaveTimerCount := 0.U
      openTxSlave := true.B
    }.elsewhen(allChannelsResponded) {
      // Reset timer and flags if all channels responded
      slaveTimerCount := 0.U
      isSlaveTimer := false.B
    }.otherwise {
      // Increment timer if not all channels responded & timer has not expired
      slaveTimerCount := slaveTimerCount + 1.U
    }
  }
  when(openTxSlave && !connTxComplete) {
    when(channelIndex < CHANNEL_NUM.U) {
      // Send connection message - first channel with TYPE_OPEN_CONNECTION, others with TYPE_NORMAL
      connTxComplete := true.B
      send_frame(TYPE_NORMAL, io.nextTxSeq, getPrevSeqNum(io.nextRxSeq), channelIndex, CREDIT_DEFAULT.U)

      // Request sequence number increment
      io.incTxSeq := true.B
      channelIndex := channelIndex + 1.U
    }.elsewhen(channelIndex === CHANNEL_NUM.U) {
      openTxSlave := false.B
    }
  }

  when(openRxSlave) {
    // Update channel response status only if not already processed
    when(connRxChannel =/= 0.U && !channelResponses(connRxChannel)) {
      channelResponses(connRxChannel) := true.B

      // Request sequence number increment if received sequence matches expected
      when(connRxSeqNum === io.nextRxSeq) {
        io.incRxSeq := true.B     // Request increment from sequence manager
      }

      // Send credit information to flow control only for new channel responses
      when(connRxChannel =/= 0.U) {
        io.incCreditValid := true.B
        io.incCreditChannel := connRxChannel
        io.incCreditAmount := (1.U << connRxCredit)
      }
    }
    // Received normal message (Ack message from master)
    when(connRxChannel === 0.U) {
      openRxSlaveNormal := true.B
    }
    openRxSlave := false.B
  }

  // Send ack_only message to master
  when(openRxSlaveNormal) {
    send_frame(TYPE_ACKONLY, io.nextTxSeq, getPrevSeqNum(io.nextRxSeq), 0.U, 0.U)

    openRxSlaveNormal := false.B
  }

  // if channel B and D credits are received, then complete the connection
  when(channelResponses(CHANNEL_B) && channelResponses(CHANNEL_D)) {
    openRxSlaveComplete := true.B
  }

  when(openRxSlaveComplete) {
    isSlaveTimer := false.B
    openTxSlave := false.B
    openRxSlave := false.B
    isConn := true.B
    connInProgress := false.B
  }

  //////////////////////////////////////////////////////////////////
  // Function to prepare packet for transmission
  def send_frame(msgType: UInt, seq_num: UInt, seq_num_ack: UInt, chan: UInt, credit: UInt) = {
    val packet = Wire(new TloePacket)
    
    // Set packet fields
    packet.ethHeader.destMAC := DEST_MAC
    packet.ethHeader.srcMAC := SRC_MAC
    packet.ethHeader.etherType := ETHER_TYPE
    
    packet.tloeHeader.vc := 0.U
    packet.tloeHeader.msgType := msgType
    packet.tloeHeader.res1 := 0.U
    packet.tloeHeader.seqNum := seq_num
    packet.tloeHeader.seqNumAck := seq_num_ack
    packet.tloeHeader.ack := 1.U
    packet.tloeHeader.res2 := 0.U
    packet.tloeHeader.chan := chan
    packet.tloeHeader.credit := credit
    
    packet.tlMsgHigh := 0.U.asTypeOf(new TLMessageHigh)
    packet.tlMsgLow := 0.U.asTypeOf(new TLMessageLow)
    
    // Convert to packet format
    val packetWithPadding = Cat(packet.asUInt, 0.U(272.W))
    
    // Store the packet in a register for the next clock cycle
    val storedPacket = RegNext(packetWithPadding)
    
    // In the next clock cycle, split into 64-bit chunks for transmission
    when(RegNext(true.B)) {
      connTxPacketVec := VecInit(Seq.tabulate(9) { i =>
        val high = 576 - (64 * i) - 1
        val low = math.max(576 - 64 * (i + 1), 0)
        storedPacket(high, low)
      } ++ Seq.fill(5)(0.U(64.W)))
      
      connTxPacketVecSize := 9.U
      connSendPacket := true.B
      connTxIndex := 0.U
    }
  }

  // Packet transmission state machine
  when(connSendPacket) {
    when(io.txready && connTxIndex < connTxPacketVecSize) {
      io.txdata := TloePacGen.toBigEndian(connTxPacketVec(connTxIndex))
      io.txvalid := true.B

      // Handle last packet differently
      when(connTxIndex === (connTxPacketVecSize - 1.U)) {
        io.txlast := true.B
        io.txkeep := "h3F".U
      }.otherwise {
        io.txlast := false.B
        io.txkeep := "hFF".U
      }

      connTxIndex := connTxIndex + 1.U
    }.elsewhen(connTxIndex >= connTxPacketVecSize) {
      // Reset all signals explicitly
      io.txdata := 0.U
      io.txvalid := false.B
      io.txlast := false.B
      io.txkeep := 0.U

      connTxIndex := 0.U
      connSendPacket := false.B
      connTxComplete := false.B
    }
  }
} 