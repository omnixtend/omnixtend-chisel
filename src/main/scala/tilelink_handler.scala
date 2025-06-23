package omnixtend

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class TileLinkHandler extends Module {
  val io = IO(new Bundle {
    /*
    // Input signals for enqueue
    val enqValid = Input(Bool())
    val enqBits = Input(UInt(80.W))
    */

    val rxPacketVec = Input(Vec(34, UInt(64.W)))  // Reduced from 68 to 34
    val rxPacketVecSize = Input(UInt(8.W))
    val doTilelinkHandler = Input(Bool())

    // Output signals to TLOEEndpoint
    val ep_rxOpcode = Output(UInt(3.W))
    val ep_rxParam = Output(UInt(4.W))
    val ep_rxSize = Output(UInt(4.W))
    val ep_rxSource = Output(UInt(26.W))
    val ep_rxAddr = Output(UInt(64.W))
    val ep_rxData = Output(UInt(512.W))
    val ep_rxValid = Output(Bool())

    /*
    // Queue status signals
    val queueFull = Output(Bool())
    val queueEmpty = Output(Bool())
    val queueCount = Output(UInt(5.W))  // 16 entries need 5 bits
    */
  })

  /*
  // Create input and output wires for the queue
  val queueIn = Wire(Decoupled(UInt(80.W)))
  val queueOut = Wire(Decoupled(UInt(80.W)))

  // Initialize queue with 16 entries
  val tlMsgBuffer = Module(new Queue(UInt(80.W), 16))
  tlMsgBuffer.io.enq <> queueIn
  tlMsgBuffer.io.deq <> queueOut
  */

  /*
  // Queue status signals
  io.queueFull := !tlMsgBuffer.io.enq.ready
  io.queueEmpty := !tlMsgBuffer.io.deq.valid
  io.queueCount := tlMsgBuffer.io.count
  */

  // Initialize outputs
  io.ep_rxOpcode := 0.U
  io.ep_rxParam := 0.U
  io.ep_rxSize := 0.U
  io.ep_rxSource := 0.U
  io.ep_rxAddr := 0.U
  io.ep_rxData := 0.U
  io.ep_rxValid := false.B

  /*
  // Connect external enqueue interface
  queueIn.valid := io.enqValid
  queueIn.bits := io.enqBits
  */

/*
  // Process messages from queue
  when(queueOut.valid) {
    val currentMsg = queueOut.bits
    
    // Extract fields from 80-bit message
    val channel = currentMsg(79, 77)  // 3 bits
    val opcode = currentMsg(76, 74)   // 3 bits
    val size = currentMsg(73, 71)     // 3 bits
    val data = currentMsg(70, 7)      // 64 bits
    val addr = currentMsg(6, 0)       // 7 bits
    
    // Process based on channel and opcode
    switch(channel) {
      is(1.U) { // Channel A
        switch(opcode) {
          is(0.U) { // PutFullData
            io.ep_rxdata := addr
            io.ep_rxvalid := true.B
          }
          is(1.U) { // PutPartialData
            io.ep_rxdata := addr
            io.ep_rxvalid := true.B
          }
          is(4.U) { // Get
            io.ep_rxdata := addr
            io.ep_rxvalid := true.B
          }
        }
      }
      is(2.U) { // Channel B
        switch(opcode) {
          is(TLMessages.PutFullData) {
            io.ep_rxdata := addr
            io.ep_rxvalid := true.B
          }
          is(TLMessages.Get) {
            io.ep_rxdata := addr
            io.ep_rxvalid := true.B
          }
        }
      }
      is(3.U) { // Channel C
        switch(opcode) {
          is(TLMessages.AccessAck) {
            io.ep_rxdata := addr
            io.ep_rxvalid := true.B
          }
          is(TLMessages.AccessAckData) {
            io.ep_rxdata := addr
            io.ep_rxvalid := true.B
          }
        }
      }
      is(4.U) { // Channel D
        switch(opcode) {
          is(TLMessages.AccessAck) {
            io.ep_rxdata := 0.U
            io.ep_rxvalid := true.B
          }
          is(TLMessages.AccessAckData) {
            io.ep_rxdata := data
            io.ep_rxvalid := true.B
          }
        }
      }
      is(5.U) { // Channel E
        io.ep_rxdata := addr
        io.ep_rxvalid := true.B
      }
    }

    // Acknowledge the message has been processed
    queueOut.ready := true.B
  }.otherwise {
    queueOut.ready := false.B
  }
*/
  val tlIdle :: tlGetMask :: tlGetTlHeader :: tlHandle :: tlDone :: Nil = Enum(5)
  val tlHandlerState = RegInit(tlIdle)

  val tlPacketVec = Reg(Vec(34, UInt(64.W)))  // Reduced from 68 to 34
  val tlPacketVecSize = RegInit(0.U(8.W))
  val tlHeader = Reg(new TLMessageHigh)
  val tlHeaderLow = Reg(new TLMessageLow)
  val mask = Reg(UInt(34.W))  // Reduced from 68 to 34
  val offset = Reg(UInt(6.W))

  val doth_tl = RegInit(false.B)
  doth_tl := io.doTilelinkHandler

  // TileLink Handler
  when(io.doTilelinkHandler) {
    // Only copy the first 34 elements to save LUTs
    for (i <- 0 until 34) {
      tlPacketVec(i) := io.rxPacketVec(i)
    }
    tlPacketVecSize := io.rxPacketVecSize
    tlHandlerState := tlGetMask
  }

  // Optimized getMask function - simplified for smaller vectors
  def getMaskOptimized(packet: Vec[UInt], size: UInt): UInt = {
    // Simplified mask extraction for smaller packets
    Mux(size <= 4.U,
      // For small packets, use a simpler approach
      Cat(packet(size-1.U)(15, 0), packet(size)(63, 16)),
      // For larger packets, use the original logic but with bounds checking
      {
        val safeSize = Mux(size > 34.U, 34.U, size)
        Cat(packet(safeSize-2.U)(15, 0), packet(safeSize-1.U)(63, 16))
      }
    )
  }

  switch(tlHandlerState) {
    is(tlIdle) {    
    }

    is(tlGetMask) {
      // Find first set bit in mask (LSB first)
      mask := getMaskOptimized(tlPacketVec, tlPacketVecSize)
      offset := PriorityEncoder(getMaskOptimized(tlPacketVec, tlPacketVecSize))
      tlHandlerState := tlGetTlHeader
    }

    is(tlGetTlHeader) {
      // Extract TileLink Message from the found position
      val tlHeaderWire = Wire(new TLMessageHigh)
      tlHeaderWire := Cat(
        TloePacGen.toBigEndian(tlPacketVec(2.U +& offset))(15, 0),
        TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(63, 16)
      ).asTypeOf(new TLMessageHigh)
      tlHeader := tlHeaderWire

      val tlHeaderLowWire = Wire(new TLMessageLow)
      tlHeaderLowWire := Cat(
        TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0),
        TloePacGen.toBigEndian(tlPacketVec(4.U +& offset))(63, 16)
      ).asTypeOf(new TLMessageLow)
      tlHeaderLow := tlHeaderLowWire

      tlHandlerState := tlHandle
    }

    is(tlHandle) {
      io.ep_rxOpcode := tlHeader.opcode
      io.ep_rxParam := tlHeader.param
      io.ep_rxSize := tlHeader.size
      io.ep_rxSource := tlHeader.source
      io.ep_rxAddr := tlHeaderLow.addr

      // Simplified channel processing - only handle the most common case
      when(tlHeader.chan === 4.U && tlHeader.opcode === TLMessages.AccessAckData) {
        // Channel D with AccessAckData - simplified data extraction
        val baseOffset = 3.U +& offset
        val dataOffset = 4.U +& offset
        
        // Use a more efficient data extraction method
        val dataSize = (1.U << tlHeader.size) - 1.U
        val extractedData = Wire(UInt(512.W))
        
        // Simplified data extraction - only handle common sizes
        when(tlHeader.size <= 3.U) {
          // For smaller sizes, use direct concatenation
          extractedData := Cat(
            TloePacGen.toBigEndian(tlPacketVec(baseOffset))(15, 0),
            TloePacGen.toBigEndian(tlPacketVec(dataOffset))(63, 16)
          )
        }.otherwise {
          // For larger sizes, use a simplified approach
          val numWords = Mux(tlHeader.size <= 5.U, 1.U << (tlHeader.size - 3.U), 8.U)
          val dataVec = Wire(Vec(8, UInt(64.W)))
          
          for (i <- 0 until 8) {
            when(i.U < numWords && (baseOffset + i.U) < 34.U) {
              dataVec(i) := TloePacGen.toBigEndian(tlPacketVec(baseOffset + i.U))
            }.otherwise {
              dataVec(i) := 0.U
            }
          }
          
          extractedData := Cat(dataVec.reverse)
        }
        
        io.ep_rxData := extractedData & dataSize
        io.ep_rxValid := true.B
      }.otherwise {
        // For other channels/opcodes, use address as data
        io.ep_rxData := tlHeaderLow.addr
        io.ep_rxValid := true.B
      }

      tlHandlerState := tlDone   
    }

    is(tlDone) {
      io.ep_rxValid := false.B
      tlHandlerState := tlIdle
    }
  }
} 
