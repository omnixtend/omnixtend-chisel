package omnixtend

import chisel3._
import chisel3.util._

/**
 * Transceiver module interfaces with the TileLink messages and manages the
 * serialization and deserialization of data for transmission and reception.
 * This module acts as a bridge between TileLink and Ethernet, handling both
 * transmission and reception of data packets.
 */
class Transceiver extends Module {
  val io = IO(new Bundle {
    // TileLink interface for transmission
    val txAddr      = Input(UInt(64.W))   // Input address for transmission
    val txData      = Input(UInt(512.W))  // Input data for transmission
    val txSize      = Input(UInt(3.W))    // Input data for transmission
    val txOpcode    = Input(UInt(3.W))    // Input opcode for transmission
    val txValid     = Input(Bool())       // Valid signal for transmission
    val txReady     = Output(Bool())      // Ready signal for transmission

    // TileLink interface for reception
    val rxData      = Output(UInt(64.W))  // Output data received
    val rxValid     = Output(Bool())      // Valid signal for received data
    val rxReady     = Input(Bool())       // Ready signal for receiver

    // Ethernet IP core interface
    val axi_rxdata  = Output(UInt(512.W))
    val axi_rxvalid = Output(Bool())
    val txdata      = Output(UInt(64.W))
    val txvalid     = Output(Bool())
    val txlast      = Output(Bool())
    val txkeep      = Output(UInt(8.W))
    val txready     = Input(Bool())
    val rxdata      = Input(UInt(64.W))
    val rxvalid     = Input(Bool())
    val rxlast      = Input(Bool())

    val ox_open     = Input(Bool())       // Signal to open OmniXtend connection
    val ox_close    = Input(Bool())       // Signal to close OmniXtend connection
    val debug1      = Input(Bool())
    val debug2      = Input(Bool())
  })

  // Connection and state-related registers
  val conn        = RegInit(0.U(8.W))   // OmniXtend Connection state

  val A_channel   = RegInit(0.U(16.W))  // Channel for tracking A-type transactions 
  val C_channel   = RegInit(0.U(16.W))  // Channel for tracking C-type transactions 
  val E_channel   = RegInit(0.U(16.W))  // Channel for tracking E-type transactions 

  // Registers for AXI-Stream transmission state
  val axi_txdata  = RegInit(0.U(64.W))
  val axi_txvalid = RegInit(false.B)
  val axi_txlast  = RegInit(false.B)
  val axi_txkeep  = RegInit(0.U(8.W))

  // Sequence tracking for transmission and acknowledgment
  val next_tx_seq = RegInit(0.U(22.W))
  val ackd_seq    = RegInit("h3FFFFF".U(22.W))
  val next_rx_seq = RegInit(0.U(22.W))

  // Registers for OmniXtend packets and packet state
  val oPacket     = RegInit(0.U(576.W))
  val nAckPacket  = RegInit(0.U(576.W))

  // Registers for received AXI-Stream data
  val axi_rxdata  = RegInit(0.U(512.W))
  val axi_rxvalid = RegInit(false.B)

  // Count of received packets and vectors to hold packet contents
  val rxcount        = RegInit(0.U(8.W))
  val rPacketVec     = RegInit(VecInit(Seq.fill(14)(0.U(64.W))))
  val rPacketVecSize = RegInit(0.U(4.W))

  // Vector for holding outgoing packets
  val txPacketVec     = RegInit(VecInit(Seq.fill(14)(0.U(64.W))))
  val txPacketVecSize = RegInit(0.U(4.W)) 

  val rxPacketReceived     = RegInit(false.B)    // Signal indicating a packet was received
  val rxPacketReceived_loc = RegInit(false.B)    // Location signal for received packet 

  val mask = RegInit(0.U(64.W))                  // Packet mask for received data

  // Connect internal signals to external IO interface for AXI transmission
  io.txvalid := axi_txvalid
  io.txdata := axi_txdata
  io.txlast := axi_txlast
  io.txkeep := axi_txkeep

  io.axi_rxdata  := axi_rxdata
  io.axi_rxvalid := axi_rxvalid

  //////////////////////////////////////////////////////////////////
  // State Machines for different packet handling

  val oidle :: opacket1_ready :: opacket1_sent :: opacket2_ready :: opacket2_sent :: opacket3_ready :: opacket3_sent :: opacket4_ready :: opacket4_sent :: opacket5_ready :: opacket5_sent :: owaiting_ack1 :: owaiting_ack2 :: osending_ack :: owaiting_ack3 :: Nil = Enum(15)
  // State register for open packet handling
  val state = RegInit(oidle)

  // State registers for control, read, and write sequences
  val cidle :: cpacket_sent :: cwaiting_ack :: Nil = Enum(3)
  val cstate = RegInit(cidle)

  val ridle :: rsendRequest :: rwaitCredit :: rwaitResponse :: rprocessResponse :: rwaitAck :: rdone :: Nil = Enum(7)
  val rstate = RegInit(ridle)

  val widle :: wsendRequest :: wwaitCredit1 :: wwaitCredit2 :: wwaitResponse :: wprocessResponse :: wwaitAck :: wdone :: Nil = Enum(8)
  val wstate = RegInit(ridle)

  // Register for packet index during sending
  val idx = RegInit(0.U(16.W))

  //////////////////////////////////////////////////////////////////
  // Packet Sending Logic

  val sendPacket = RegInit(false.B)
  val txComplete = RegInit(false.B)

  // State machine for sending packets via AXI-Stream interface
  when (sendPacket) {
    when (idx < txPacketVecSize) {
      axi_txdata  := TloePacGen.toBigEndian(txPacketVec(idx))  // Store current packet data in axi_txdata
      axi_txvalid := true.B                                    // Set valid signal
      idx         := idx + 1.U                                 // Move to next packet

      // Check if this is the last packet
      when (idx === (txPacketVecSize - 1.U)) {
        axi_txlast := true.B         // Set last packet flag
        axi_txkeep := 0x3F.U         // Last packet flag
        idx        := 20.U           // Reset index
      } .otherwise {
        axi_txlast := false.B
        axi_txkeep := 0xFF.U
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
  // OmniXtend Connection Handling
  when (io.ox_open) {
    when (state === oidle) {
      state := opacket1_ready
      //oPacket := OXPacket.openConnection(next_tx_seq+1.U, 2.U, 9.U)  // Credit 9
      oPacket := OXPacket.openConnection(next_tx_seq, 1.U, 9.U)  // Packet with credit of 9
    }
  }

  when (state === opacket1_ready) {
    txPacketVec := VecInit(Seq.tabulate(9) { i =>
      val packetWidth = 576
      val high = packetWidth - (64 * i) - 1
      val low = math.max(packetWidth - 64 * (i + 1), 0)
      oPacket(high, low)
    } ++ Seq.fill(5)(0.U(64.W)))

    next_tx_seq := next_tx_seq + 1.U

    txPacketVecSize := 9.U

    sendPacket := true.B
    txComplete := false.B
    state := opacket1_sent
  }

  when (state === opacket1_sent) {
    when (txComplete) {
      state := opacket2_ready
      //oPacket := OXPacket.openConnection(next_tx_seq+1.U, 4.U, 9.U)  // Credit 9
      oPacket := OXPacket.openConnection(next_tx_seq, 2.U, 9.U)  // Credit 9

      txComplete := false.B
    }
  }

  when (state === opacket2_ready) {
    txPacketVec := VecInit(Seq.tabulate(9) { i =>
      val packetWidth = 576
      val high = packetWidth - (64 * i) - 1
      val low = math.max(packetWidth - 64 * (i + 1), 0)
      oPacket(high, low)
    } ++ Seq.fill(5)(0.U(64.W)))

    next_tx_seq := next_tx_seq + 1.U

    txPacketVecSize := 9.U

    sendPacket := true.B
    txComplete := false.B
    state := opacket2_sent
  }

  when (state === opacket2_sent) {
    when (txComplete) {
      state := opacket3_ready
      //oPacket := OXPacket.openConnection(next_tx_seq+1.U, 4.U, 9.U)  // Credit 9
      oPacket := OXPacket.openConnection(next_tx_seq, 3.U, 9.U)  // Credit 9

      txComplete := false.B
    }
  }

  when (state === opacket3_ready) {
    txPacketVec := VecInit(Seq.tabulate(9) { i =>
      val packetWidth = 576
      val high = packetWidth - (64 * i) - 1
      val low = math.max(packetWidth - 64 * (i + 1), 0)
      oPacket(high, low)
    } ++ Seq.fill(5)(0.U(64.W)))

    next_tx_seq := next_tx_seq + 1.U

    txPacketVecSize := 9.U

    sendPacket := true.B
    txComplete := false.B
    state := opacket3_sent
  }

  when (state === opacket3_sent) {
    when (txComplete) {
      state := opacket4_ready
      oPacket := OXPacket.openConnection(next_tx_seq, 4.U, 9.U)  // Credit 9

      txComplete := false.B
    }
  }

  when (state === opacket4_ready) {
    txPacketVec := VecInit(Seq.tabulate(9) { i =>
      val packetWidth = 576
      val high = packetWidth - (64 * i) - 1
      val low = math.max(packetWidth - 64 * (i + 1), 0)
      oPacket(high, low)
    } ++ Seq.fill(5)(0.U(64.W)))

    next_tx_seq := next_tx_seq + 1.U

    txPacketVecSize := 9.U

    sendPacket := true.B
    txComplete := false.B
    state := opacket4_sent
  }

  when (state === opacket4_sent) {
    when (txComplete) {
      state := opacket5_ready
      //oPacket := OXPacket.openConnection(next_tx_seq+1.U, 4.U, 9.U)  // Credit 9
      oPacket := OXPacket.openConnection(next_tx_seq, 5.U, 9.U)  // Credit 9

      txComplete := false.B
    }
  }

  when (state === opacket5_ready) {
    txPacketVec := VecInit(Seq.tabulate(9) { i =>
      val packetWidth = 576
      val high = packetWidth - (64 * i) - 1
      val low = math.max(packetWidth - 64 * (i + 1), 0)
      oPacket(high, low)
    }) ++ Seq.fill(5)(0.U(64.W))

    next_tx_seq := next_tx_seq + 1.U

    txPacketVecSize := 9.U

    sendPacket := true.B
    txComplete := false.B
    state := opacket5_sent
  }

  when (state === opacket5_sent) {
    when (txComplete) {
      state := owaiting_ack1

      txComplete := false.B
    }
  }

  // if Ackonly Packet
  val rx_seq_num = RegInit(1.U(2.W))

  when (state === owaiting_ack1) {
    //TODO handling recv packet
    when (rx_seq_num === 1.U) {
      state := owaiting_ack2
      rx_seq_num := 2.U

      //TODO Not here
      ackd_seq := 2.U
    }
  }

  when (state === owaiting_ack2) {
    //TODO handling recv packet
    when (rx_seq_num === 2.U) {
      state := osending_ack
      rx_seq_num := 3.U

      nAckPacket := OXPacket.normalAck(next_tx_seq, ackd_seq, 1.U, 0.U, 0.U)  //TODO ack number
    }
  }

  when (state === osending_ack) {

    txPacketVec := VecInit(Seq.tabulate(9) { i =>
      val packetWidth = 576
      val high = packetWidth - (64 * i) - 1
      val low = math.max(packetWidth - 64 * (i + 1), 0)
      nAckPacket(high, low)
    } ++ Seq.fill(5)(0.U(64.W)))

    next_tx_seq := next_tx_seq + 1.U

    txPacketVecSize := 9.U

    sendPacket := true.B
    txComplete := false.B
    state := owaiting_ack3
  }

  when (state === owaiting_ack3 && rx_seq_num === 3.U) {
    //TODO handling recv packet
    when (txComplete && rx_seq_num === 3.U) {
      state := oidle

      //TODO not here
      next_tx_seq := 6.U
      ackd_seq    := 5.U
      next_rx_seq := 3.U
    }
  }

  //////////////////////////////////////////////////////////////////
  // Close Connection Handling

  val cPacket     = RegInit(0.U(576.W))

  when (io.ox_close) {
    cstate := cpacket_sent 
    cPacket := OXPacket.closeConnection(next_tx_seq +1.U)
  }

  switch (cstate) {
    is (cpacket_sent) {
      txPacketVec := VecInit(Seq.tabulate(9) { i =>
        val packetWidth = 576
        val high = packetWidth - (64 * i) - 1
        val low = math.max(packetWidth - 64 * (i + 1), 0)
        cPacket(high, low)
      } ++ Seq.fill(5)(0.U(64.W)))

      next_tx_seq := next_tx_seq + 1.U

      txPacketVecSize := 9.U

      sendPacket := true.B
      txComplete := false.B
      cstate := cwaiting_ack
    }

    is (cwaiting_ack) {
      //TODO handling recv packet
      when (txComplete) {
        cstate := cidle

        txComplete := false.B
      }
    }
  }

  //////////////////////////////////////////////////////////////////
  // RX - Receive Path for AXI-Stream Data

  // If io.rxvalid signal is low, indicating no incoming data, reset rxcount
  when (!io.rxvalid) {
    rxcount := 0.U
  }
 
  // When io.rxvalid is high, data is being received
  when (io.rxvalid) {
    rxcount := rxcount + 1.U

    // Store incoming data at the current rxcount index in rPacketVec vector
    rPacketVec(rxcount) := io.rxdata

    // Check if io.rxlast is high, signaling the end of the packet
    when (io.rxlast) { 

      // Packet drop condition: Compare received sequence with the expected sequence
      when (next_rx_seq === TloePacGen.getSeqNum(rPacketVec)) {

        // Check if we are in one of the receiving states for an expected packet
        when (rstate === rwaitCredit || rstate === rwaitResponse || rstate === rwaitAck) {
          rxPacketReceived := true.B
        }

        // Similarly, check the write states for wstate
        when (wstate === wwaitCredit1 || wstate === wwaitCredit2 || wstate === wwaitResponse || wstate === wwaitAck) {
          rxPacketReceived := true.B
        }

        // Update rPacketVecSize to store the number of received segments
        rPacketVecSize := rxcount + 1.U
      }

      // Reset rxcount to prepare for the next incoming packet
      rxcount := 0.U
    }
  }
 

  //////////////////////////////////////////////////////////////////
  // TX - Transmission Path for Packet Processing and Queue Management

  // Register for storing read (rPacket) and write (wPacket) packets
  val rPacket     = RegInit(0.U(576.W))
  val wPacket     = RegInit(0.U(896.W))

  rPacket := 0.U  // Initialize rPacket to zero at the start

  // Queues for storing address, opcode, size, and data for transmission
  val addrQueue   = Module(new Queue(UInt(64.W), 16))
  val opcodeQueue = Module(new Queue(UInt(3.W), 16))
  val sizeQueue   = Module(new Queue(UInt(3.W), 16))
  val dataQueue   = Module(new Queue(UInt(512.W), 16))

  // Default queue port values: Set all enqueue signals to false and data bits to zero
  addrQueue.io.enq.valid   := false.B
  addrQueue.io.enq.bits    := 0.U
  addrQueue.io.deq.ready   := false.B

  opcodeQueue.io.enq.valid := false.B
  opcodeQueue.io.enq.bits  := 0.U
  opcodeQueue.io.deq.ready := false.B

  sizeQueue.io.enq.valid   := false.B
  sizeQueue.io.enq.bits    := 0.U
  sizeQueue.io.deq.ready   := false.B

  dataQueue.io.enq.valid   := false.B
  dataQueue.io.enq.bits    := 0.U
  dataQueue.io.deq.ready   := false.B

  // Enqueue data into the respective queues when txValid is asserted
  when(io.txValid) {
    addrQueue.io.enq.bits    := io.txAddr      // Store address in addrQueue
    addrQueue.io.enq.valid   := true.B

    opcodeQueue.io.enq.bits  := io.txOpcode  // Store opcode in opcodeQueue
    opcodeQueue.io.enq.valid := true.B

    sizeQueue.io.enq.bits    := io.txSize      // Store size in sizeQueue
    sizeQueue.io.enq.valid   := true.B

    dataQueue.io.enq.bits    := io.txData      // Store data in dataQueue
    dataQueue.io.enq.valid   := true.B
  }

  // Only set txReady to true when all queues are ready to accept data
  io.txReady := addrQueue.io.enq.ready && opcodeQueue.io.enq.ready && sizeQueue.io.enq.ready && dataQueue.io.enq.ready

  val tx_size = RegInit(0.U(3.W))

  // Check that all states are idle and queues have valid data to dequeue
  when (state === 0.U && cstate === 0.U && rstate === 0.U && wstate === 0.U && 
        addrQueue.io.deq.valid && opcodeQueue.io.deq.valid && sizeQueue.io.deq.valid && dataQueue.io.deq.valid) {

    // Dequeue the next data values from each queue
    val nextAddr   = addrQueue.io.deq.bits
    val nextOpcode = opcodeQueue.io.deq.bits
    val nextSize   = sizeQueue.io.deq.bits
    val nextData   = dataQueue.io.deq.bits

    // Process dequeued data according to opcode (READ or WRITE)
    when (nextOpcode === 4.U) {         // READ operation
      rPacket := OXPacket.readPacket(nextAddr, next_tx_seq, next_rx_seq, nextSize)
      rstate := rsendRequest
      tx_size := nextSize
    }.elsewhen (nextOpcode === 0.U) {   // WRITE operation
      wPacket := OXPacket.writePacket(nextAddr, nextData, next_tx_seq, next_rx_seq, nextSize)
      wstate := wsendRequest
      tx_size := nextSize
    }.otherwise {
      // TODO Additional opcode processing can be added here if needed 
    }

    // Mark the queues as ready to dequeue the processed items
    addrQueue.io.deq.ready   := true.B
    opcodeQueue.io.deq.ready := true.B
    sizeQueue.io.deq.ready   := true.B
    dataQueue.io.deq.ready   := true.B
  }.otherwise {
    // If the queues are not ready, or if the state is not idle, set dequeue ready to false
    addrQueue.io.deq.ready   := false.B
    opcodeQueue.io.deq.ready := false.B
    sizeQueue.io.deq.ready   := false.B
    dataQueue.io.deq.ready   := false.B
  }

  //////////////////////////////////////////////////////////////////
  // READ - Processing Path for Read Requests

  // Registers to store packet locations within the read state
val packetloc = RegInit(0.U(6.W))
val packetloc2 = RegInit(0.U(6.W))
val packetloc3 = RegInit(0.U(64.W))
val packetloc4 = RegInit(0.U(64.W))

  // Switch statement handling different stages of the read state (rstate)
  switch (rstate) {
    is (rsendRequest) {
      // Prepare the read packet by dividing rPacket into 64-bit segments and storing in txPacketVec
      txPacketVec := VecInit(Seq.tabulate(9) { i => 
        rPacket(576 - (64 * i) - 1, 576 - 64 * (i + 1))
      } ++ Seq.fill(5)(0.U(64.W)))

      next_tx_seq := next_tx_seq + 1.U  // Increment TX sequence number

      txPacketVecSize := 9.U            // Set the size of the packet vector

      sendPacket := true.B              // Indicate that packet is ready to send
      txComplete := false.B             // Reset transmission complete flag

      rstate := rwaitCredit             // Move to wait for credit acknowledgment
    }

    is (rwaitCredit) {
      // Wait for packet transmission to complete and acknowledgment to be received
      when (txComplete && rxPacketReceived) {
        // Update acked_seq TODO If not drop

        ackd_seq    := TloePacGen.getSeqNumAck(rPacketVec)     // Get acknowledgment sequence number
        next_rx_seq := TloePacGen.getSeqNum(rPacketVec) + 1.U  // Update next RX sequence 

        // TODO Do something with Channel A

        rxPacketReceived := false.B      // Reset received packet flag

        for (i <- 0 until 14) { txPacketVec(i) := 0.U }  // Initialize txPacketVec

        txComplete := false.B
        rstate := rwaitResponse          // Transition to wait for a response
      }
    }

    is (rwaitResponse) {
      // Process response packet if received
      when (rxPacketReceived) {
        // Update packet location registers based on received data
        packetloc := packetloc + 3.U
        packetloc2 := PriorityEncoder(Reverse(TloePacGen.getMask(rPacketVec, rPacketVecSize)))
        packetloc3 := TloePacGen.getMask(rPacketVec, rPacketVecSize)
        packetloc4 := Reverse(TloePacGen.getMask(rPacketVec, rPacketVecSize))(15,0)

        rxPacketReceived_loc := true.B  // Set flag indicating location update
      }

      // Handle the received data based on the TX size, creating the response packet accordingly
      when (rxPacketReceived_loc) {
        // Update acked_seq TODO If not drop
        ackd_seq := TloePacGen.getSeqNumAck(rPacketVec)       // Update acknowledgment sequence
        next_rx_seq := TloePacGen.getSeqNum(rPacketVec) + 1.U // Update next RX sequence


        // Create AXI-Stream data for different sizes based on tx_size
        switch (tx_size) {
          is (1.U) {
            axi_rxdata := Cat(TloePacGen.toBigEndian(rPacketVec(packetloc2))(15, 0), 0.U(496.W)) 
          }

          is (2.U) {
            axi_rxdata := Cat(
              (TloePacGen.toBigEndian(rPacketVec(packetloc3)))(15, 0), 
              (TloePacGen.toBigEndian(rPacketVec(packetloc4 + 1.U)))(63, 48),
              0.U(480.W)) 
          }

          is (3.U) {
            axi_rxdata := Cat(
              (TloePacGen.toBigEndian(rPacketVec(packetloc)))(15, 0), 
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 1.U)))(63, 16), 
              0.U(448.W))
          }

          is (4.U) {
            axi_rxdata := Cat(
              (TloePacGen.toBigEndian(rPacketVec(packetloc)))(15, 0), 
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 1.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 2.U)))(63, 16), 
              0.U(384.W))
          }

          is (5.U) {
            axi_rxdata := Cat(
              (TloePacGen.toBigEndian(rPacketVec(packetloc)))(15, 0), 
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 1.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 2.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 3.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 4.U)))(63, 16), 
              0.U(256.W))
          }

          is (6.U) {
            axi_rxdata := Cat(
              (TloePacGen.toBigEndian(rPacketVec(packetloc)))(15, 0), 
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 1.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 2.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 3.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 4.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 5.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 6.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 7.U)))(63, 0),
              (TloePacGen.toBigEndian(rPacketVec(packetloc + 8.U)))(63, 16)) 
          }
        }
        axi_rxvalid := true.B            // Set valid flag for AXI data output

        rxPacketReceived     := false.B 
        rxPacketReceived_loc := false.B

        mask      := 0.U  // Reset mask
        packetloc := 0.U  // Reset packet location

        // Generate a normal acknowledgment packet
        nAckPacket := OXPacket.normalAck(next_tx_seq, next_rx_seq + 1.U, 1.U, 4.U, 4.U)  //TODO ack number

        rstate := rprocessResponse  // Move to process response state
      }
    }

    is (rprocessResponse) {
      axi_rxvalid := false.B  // Clear AXI RX valid flag

      // Populate txPacketVec with acknowledgment packet data
      txPacketVec := VecInit(Seq.tabulate(9) { i => 
        nAckPacket(576 - (64 * i) - 1, 576 - 64 * (i + 1))
      } ++ Seq.fill(5)(0.U(64.W)))
      txPacketVecSize := 9.U  // Set packet vector size

      next_tx_seq := next_tx_seq + 1.U  // Increment TX sequence

      sendPacket := true.B   // Indicate packet is ready to send
      txComplete := false.B

      rstate := rwaitAck  // Move to wait for acknowledgment state
    }

    is (rwaitAck) {
      // Wait for acknowledgment receipt
      when (txComplete && rxPacketReceived) {
        // Update acked_seq TODO If not drop

        ackd_seq := TloePacGen.getSeqNum(rPacketVec)   // Update acknowledgment sequence

        // TODO check exptected Ack
        rxPacketReceived := false.B
        txComplete := false.B

        rstate := rdone  // Move to done state
      }
    }

    is (rdone) {
      // Reset to idle state after processing is complete
      rstate := ridle
    }
  }

  //////////////////////////////////////////////////////////////////
  // WRITE - Packet Handling for Write Operations

  switch (wstate) {
    // State for sending a write request
    is (wsendRequest) {

      // Populate the `txPacketVec` with `wPacket` data based on the transaction size
      when (tx_size === 6.U) {
        // If tx_size is 6, prepare a full 896-bit packet, split into 64-bit chunks
        // Fill `txPacketVec` with the 14 words of `wPacket` (896 bits), most significant bits first
        txPacketVec := VecInit(Seq.tabulate(14)(i => wPacket(896 - (64 * i) - 1, 896 - 64 * (i + 1))))
        txPacketVecSize := 14.U
      } .otherwise {
        // For tx_size from 1 to 5, use only the lower 640 bits of `wPacket`
        // Fill `txPacketVec` with the 10 words of `wPacket`, leaving 4 unused entries (filled with 0s)
        txPacketVec := VecInit(Seq.tabulate(10)(i => wPacket(640 - (64 * i) - 1, math.max(640 - 64 * (i + 1), 0))) ++ Seq.fill(4)(0.U(64.W)))
        txPacketVecSize := 10.U
      }

      // Increment the next transmission sequence number after preparing the packet
      next_tx_seq := next_tx_seq + 1.U

      sendPacket := true.B   // Signal to begin transmission of the packet
      txComplete := false.B  // Clear the transmission complete flag

      wstate := wwaitCredit1 // Transition to `wwaitCredit1` to wait for credit acknowledgment
    }

    // State for waiting for the first credit acknowledgment
    is (wwaitCredit1) {
      when (rxPacketReceived) {
        // If a packet is received, update sequence tracking based on acknowledgment packet
        ackd_seq := TloePacGen.getSeqNumAck(rPacketVec)        // Retrieve acknowledged sequence number
        next_rx_seq := TloePacGen.getSeqNum(rPacketVec) + 1.U 

       // TODO Update credits of A channel

        rxPacketReceived := false.B  // Clear the received packet flag

        wstate := wwaitCredit2       // Transition to `wwaitCredit2` for the second credit acknowledgment
      }
    }

    // State for waiting for the second credit acknowledgment
    is (wwaitCredit2) {
      when (rxPacketReceived) {
        // If a packet is received, update sequence tracking again
        ackd_seq := TloePacGen.getSeqNumAck(rPacketVec)        // Retrieve acknowledged sequence number
        next_rx_seq := TloePacGen.getSeqNum(rPacketVec) + 1.U 

        // TODO Update credits of A channel

        // If there’s a non-zero mask, prepare a normal acknowledgment packet
        when (TloePacGen.getMask(rPacketVec, rPacketVecSize) =/= 0.U) {
/*
      when (Cat(
        TloePacGen.toBigEndian(rPacketVec(rPacketVecSize-1.U))(15, 0),
        TloePacGen.toBigEndian(rPacketVec(rPacketVecSize))(63, 16)) =/= 0.U) {
*/
          nAckPacket := OXPacket.normalAck(next_tx_seq, next_rx_seq + 1.U, 1.U, 4.U, 4.U)  //TODO ack number

          axi_rxdata := 0.U      // Clear AXI RX data
          axi_rxvalid := true.B  // Set AXI RX valid signal

          rPacketVecSize := 0.U  // Reset the packet vector size

          wstate := wprocessResponse  // Move to `wprocessResponse` to handle the response
        }.otherwise {
          wstate := wwaitResponse     // Otherwise, transition to `wwaitResponse`
        }
        rxPacketReceived := false.B   // Clear received packet flag
      }
    }

    // State for waiting to process the response packet
    is (wwaitResponse) {
      // On completing TX and receiving an RX packet, update the acknowledgment sequence
      when (txComplete && rxPacketReceived) {
        // Update acked_seq TODO If not drop
        ackd_seq := TloePacGen.getSeqNumAck(rPacketVec)        // Retrieve acknowledged sequence number
        next_rx_seq := TloePacGen.getSeqNum(rPacketVec) + 1.U 

        // TODO
        axi_rxdata := 0.U      // Clear AXI RX data
        axi_rxvalid := true.B  // Set AXI RX valid signal

        rxPacketReceived := false.B  // Clear RX packet received flag
        txComplete := false.B        // TX complete flag

        // Prepare a normal acknowledgment packet for response
        nAckPacket := OXPacket.normalAck(next_tx_seq, next_rx_seq + 1.U, 1.U, 4.U, 4.U)  //TODO ack number

        wstate := wprocessResponse  // Move to `wprocessResponse` state
      }
    }

    // State for processing the response packet and preparing for the next TX
    is (wprocessResponse) {
      axi_rxvalid := false.B
      // make packet
/*
      txPacketVec := VecInit(Seq.tabulate(9) (i => {
        val packetWidth = OXPacket.readPacket(io.txAddr, next_tx_seq, ackd_seq, io.txSize).getWidth
        val high = packetWidth - (64 * i) - 1
	    val low = math.max(packetWidth - 64 * (i + 1), 0)

        wPacket (high, low)
      }))
*/

/*

      when (tx_size === 6.U) {
        // txSize가 6일 경우, 전체 896비트를 64비트씩 나눠서 상위 비트를 먼저 저장
        txPacketVec := VecInit(Seq.tabulate(14)(i => wPacket(896 - (64 * i) - 1, 896 - 64 * (i + 1))))
        txPacketVecSize := 14.U
      } .otherwise {
        // txSize가 1~5일 경우, 하위 640비트만 사용 (wPacket의 하위 비트부터 저장)
        txPacketVec := VecInit(Seq.tabulate(10)(i => wPacket(640 - (64 * i) - 1, math.max(640 - 64 * (i + 1), 0))) ++ Seq.fill(4)(0.U(64.W)))
        txPacketVecSize := 10.U
      }
*/

      // Fill `txPacketVec` with the acknowledgment packet (`nAckPacket`) data
      txPacketVec := VecInit(Seq.tabulate(9) { i => 
        nAckPacket(576 - (64 * i) - 1, 576 - 64 * (i + 1))
      } ++ Seq.fill(5)(0.U(64.W)))  // Add extra 5 zero entries if not used
      txPacketVecSize := 9.U

      next_tx_seq := next_tx_seq + 1.U  // Increment TX sequence number

      sendPacket := true.B   // Signal to start packet transmission
      txComplete := false.B  // Reset transmission completion flag

      wstate := wwaitAck  // Transition to `wwaitAck` to wait for acknowledgment
    }

    // State for waiting for the acknowledgment packet
    is (wwaitAck) {
      // If TX is complete and a response is received, update acknowledgment sequence
      when (txComplete && rxPacketReceived) {
        // Update acked_seq TODO If not drop

        ackd_seq := TloePacGen.toBigEndian(rPacketVec(2))(47, 26)   // Update acknowledgment sequence
/*
        next_rx_seq := Cat(
          (TloePacGen.toBigEndian(rPacketVec(1)))(5, 0), 
          (TloePacGen.toBigEndian(rPacketVec(2)))(63, 48)
        ) + 1.U      // seqNum
*/

        // TODO check exptected Ack

        rxPacketReceived := false.B  // Clear RX packet received flag
        txComplete       := false.B  // Reset TX complete flag

        wstate := wdone  // Transition to `wdone` state upon successful acknowledgment
      }
    }

    // Final state after handling the write operation
    is (wdone) {
      wstate := ridle  // Return `wstate` to idle, ready for the next write request
    }
  }

  /////////////////////////////////////////////////
  /////////////////////////////////////////////////
  // Connect to Simulator with Endpoint module

  // Queue for outgoing TLoE packets
  // 16 entries of TloePacket type
  //val txQueue = Module(new Queue(UInt(640.W), 16))
  val txQueue = Module(new Queue(UInt(64.W), 16))
  
  // Queue for incoming TLoE packets
  // 16 entries of TloePacket type
  val rxQueue = Module(new Queue(UInt(640.W), 16))

  val tx_packet_vec = RegInit(VecInit(Seq.fill(9)(0.U(64.W))))
  val txCount     = RegInit(0.U(10.W))

  val tmpP = RegInit(0.U(576.W))
  val tloePacket = RegInit(2.U(576.W))

  // Default values for transmission queue
  txQueue.io.enq.bits  := 0.U
  txQueue.io.enq.valid := false.B
  io.txReady           := false.B

  // Default values for reception queue
  rxQueue.io.enq.bits  := 0.U
  rxQueue.io.enq.valid := false.B
  io.rxData            := 0.U
  io.rxValid           := false.B

  val txAddrReg = RegNext(io.txAddr)

  // Enqueue a TLoE packet into the transmission queue when txValid is asserted
  when (io.txValid) {

    // Create a TLoE packet using input address, data, and opcode
    tloePacket := OXPacket.readPacket(txAddrReg, next_tx_seq, ackd_seq, io.txSize)

    tx_packet_vec := VecInit(Seq.tabulate(9) (i => {
      //val packetWidth = OXPacket.createOXPacket(io.txAddr, next_tx_seq, ackd_seq).getWidth
      val packetWidth = 576
      val high = packetWidth - (64 * i) - 1
      val low = math.max(packetWidth - 64 * (i + 1), 0)

      tloePacket (high, low)
    }))

    //tmpP := tloePacket
    txQueue.io.enq.bits := txAddrReg
    txCount := 1.U
  }

  // Enqueue the TLoE packet into txQueue when txValid is high
  when(txCount > 0.U && txCount < 10.U) {
    txQueue.io.enq.bits := txPacketVec(txCount - 1.U)
    txCount := txCount + 1.U
  } .elsewhen (txCount === 10.U) {
    txQueue.io.enq.valid := 1.U
    txCount := txCount + 1.U
  } .elsewhen (txCount === (10+ 1).U) {
    // Reset signals after transmission
    txCount := 0.U
  }
 
  txQueue.io.enq.valid := io.txValid
  io.txReady           := txQueue.io.enq.ready

  // Instantiate the Endpoint module
  val endpoint = Module(new Endpoint)

  // Connect txQueue to the Endpoint for transmission
  endpoint.io.txQueueData.bits  := txQueue.io.deq.bits
  endpoint.io.txQueueData.valid := txQueue.io.deq.valid
  txQueue.io.deq.ready          := endpoint.io.txQueueData.ready

  // Handle rxQueue and deserialize data
  endpoint.io.rxQueueData.ready := rxQueue.io.enq.ready

  // Enqueue received data into rxQueue when Endpoint valid signal is high
  when(endpoint.io.rxQueueData.valid) {
    rxQueue.io.enq.bits  := endpoint.io.rxQueueData.bits
    rxQueue.io.enq.valid := true.B
  }

  // Dequeue data from rxQueue and output when rxReady is high
  when(rxQueue.io.deq.valid && io.rxReady) {
    val rxPacket = rxQueue.io.deq.bits
    io.rxData            := rxPacket
    io.rxValid           := true.B
    rxQueue.io.deq.ready := io.rxReady
  }.otherwise {
    io.rxValid           := false.B
    rxQueue.io.deq.ready := false.B
  }
}
