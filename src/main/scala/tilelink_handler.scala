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

    val rxPacketVec = Input(Vec(68, UInt(64.W)))
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
  val tlHandlerState = dontTouch(RegInit(tlIdle))

  val tlPacketVec = dontTouch(Reg(Vec(68, UInt(64.W))))
  val tlPacketVecSize = dontTouch(RegInit(0.U(8.W)))
//  tlPacketVec := io.rxPacketVec
//  tlPacketVecSize := io.rxPacketVecSize

  val tlHeader = dontTouch(Reg(new TLMessageHigh))
  val tlHeaderLow = dontTouch(Reg(new TLMessageLow))

  val doth_tl = dontTouch(RegInit(false.B))
  doth_tl := io.doTilelinkHandler

  val mask = dontTouch(Reg(UInt(68.W)))
  val offset = dontTouch(Reg(UInt(6.W)))

  // TileLink Handler
  when(io.doTilelinkHandler) {
    tlPacketVec := io.rxPacketVec
    tlPacketVecSize := io.rxPacketVecSize
    
    tlHandlerState := tlGetMask
  }

  val readDataDebug = dontTouch(Reg(UInt(64.W)))
  val writeDataDebug = dontTouch(Reg(UInt(64.W)))

  switch(tlHandlerState) {
    is(tlIdle) {    
    }

    is(tlGetMask) {
      // Find first set bit in mask (LSB first)
      mask := TloePacGen.getMask(tlPacketVec, tlPacketVecSize)
      offset := PriorityEncoder(TloePacGen.getMask(tlPacketVec, tlPacketVecSize))  // Remove Reverse to search from LSB

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

      // Channel별 처리
      switch(tlHeader.chan) {
        // Channel A (Client -> Manager)
        is(1.U) {
          switch(tlHeader.opcode) {
            is(0.U) { // PutFullData
              
              io.ep_rxData := tlHeaderLow.addr
              io.ep_rxValid := true.B
            }
            is(1.U) { // PutPartialData
              // Partial Write 요청 처리
              io.ep_rxData := tlHeaderLow.addr
              io.ep_rxValid := true.B
            }
            is(4.U) { // Get
              // Read 요청 처리
              io.ep_rxData := tlHeaderLow.addr
              io.ep_rxValid := true.B
            }
          }
        }

        // Channel B (Manager -> Client)
        is(2.U) {
          switch(tlHeader.opcode) {
            is(TLMessages.PutFullData) { // PutFullData
              io.ep_rxData := tlHeaderLow.addr
              io.ep_rxValid := true.B
            }
            is(TLMessages.Get) { // Get
              io.ep_rxData := tlHeaderLow.addr
              io.ep_rxValid := true.B
            }
          }
        }

        // Channel C (Client -> Manager)
        is(3.U) {
          switch(tlHeader.opcode) {
            is(TLMessages.AccessAck) { // AccessAck
              // Access 응답 처리
              io.ep_rxData := tlHeaderLow.addr
              io.ep_rxValid := true.B
            }
            is(TLMessages.AccessAckData) { // AccessAckData
              // Data와 함께 Access 응답 처리
              io.ep_rxData := tlHeaderLow.addr
              io.ep_rxValid := true.B
            }
          }
        }

        // Channel D (Manager -> Client)
        is(4.U) {
          switch(tlHeader.opcode) {
            is(TLMessages.AccessAck) { // AccessAck
              io.ep_rxData := 0.U // AccessAck는 데이터가 없음
            }
            is(TLMessages.AccessAckData) { // AccessAckData
              switch(tlHeader.size) {
                is(0.U) {
                  io.ep_rxData := TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 8)
                }

                is(1.U) {
                  io.ep_rxData := TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0)
                }

                is(2.U) {
                  io.ep_rxData := Cat(
                    TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0),
                    TloePacGen.toBigEndian(tlPacketVec(4.U +& offset))(63, 48) 
                  )
                }

                is(3.U) {
                  io.ep_rxData := Cat(
                    TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0),
                    TloePacGen.toBigEndian(tlPacketVec(4.U +& offset))(63, 16)
                  )
                }

                is(4.U) {
                  io.ep_rxData := Cat(
                    TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0),
                    TloePacGen.toBigEndian(tlPacketVec(4.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(5.U +& offset))(63, 16)
                  )
                }

                is(5.U) {
                  io.ep_rxData := Cat(
                    TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0),
                    TloePacGen.toBigEndian(tlPacketVec(4.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(5.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(6.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(7.U +& offset))(63, 16)
                  )
                }

                is(6.U) {
                  io.ep_rxData := Cat(
                    TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0),
                    TloePacGen.toBigEndian(tlPacketVec(4.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(5.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(6.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(7.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(8.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(9.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(10.U +& offset))(63, 0),
                    TloePacGen.toBigEndian(tlPacketVec(11.U +& offset))(63, 16)
                  )
                }
              }
            }
          }
        }
        
        // Channel E (Client -> Manager)
        is(5.U) {
          // Grant 응답 처리
          io.ep_rxData := tlHeaderLow.addr
          io.ep_rxValid := true.B
        }
      }
      io.ep_rxValid := true.B

      // 처리 완료 후 플래그 초기화
      tlHandlerState := tlDone   
    }

    is(tlDone) {
      io.ep_rxValid := false.B
      tlHandlerState := tlIdle
    }
  }
} 
