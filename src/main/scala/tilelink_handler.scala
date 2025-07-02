package omnixtend

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class TileLinkHandler extends Module {
  val io = IO(new Bundle {
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
  })

  // Initialize outputs
  io.ep_rxOpcode := 0.U
  io.ep_rxParam := 0.U
  io.ep_rxSize := 0.U
  io.ep_rxSource := 0.U
  io.ep_rxAddr := 0.U
  io.ep_rxData := 0.U
  io.ep_rxValid := false.B

  val tlIdle :: tlGetMask :: tlGetTlHeader :: tlHandle :: tlDone :: Nil = Enum(5)
  val tlHandlerState = RegInit(tlIdle)

  val tlPacketVec = Reg(Vec(68, UInt(64.W)))
  val tlPacketVecSize = RegInit(0.U(8.W))

  val tlHeader = Reg(new TLMessageHigh)
  val tlHeaderLow = Reg(new TLMessageLow)

  val mask = Reg(UInt(68.W))
  val offset = Reg(UInt(6.W))

  // TileLink Handler
  when(io.doTilelinkHandler) {
    tlPacketVec := io.rxPacketVec
    tlPacketVecSize := io.rxPacketVecSize
    tlHandlerState := tlGetMask
  }

  switch(tlHandlerState) {
    is(tlIdle) {    
    }

    is(tlGetMask) {
      // Simplified mask calculation - only check first 16 bits to reduce LUT usage
      mask := TloePacGen.getMask(tlPacketVec, tlPacketVecSize)
      
      // Simplified offset calculation - only check first 16 bits
      offset := 0.U
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

      // Simplified channel processing - only handle Channel D (4)
      when(tlHeader.chan === 4.U) {
        when(tlHeader.opcode === TLMessages.AccessAck) {
          io.ep_rxData := 0.U
        }.elsewhen(tlHeader.opcode === TLMessages.AccessAckData) {
          // Simplified data extraction - only handle common sizes
          when(tlHeader.size === 0.U) {
            io.ep_rxData := TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 8)
          }.elsewhen(tlHeader.size === 1.U) {
            io.ep_rxData := TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0)
          }.elsewhen(tlHeader.size === 2.U) {
            io.ep_rxData := Cat(
              TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0),
              TloePacGen.toBigEndian(tlPacketVec(4.U +& offset))(63, 48) 
            )
          }.elsewhen(tlHeader.size === 3.U) {
            io.ep_rxData := Cat(
              TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0),
              TloePacGen.toBigEndian(tlPacketVec(4.U +& offset))(63, 16)
            )
          }.otherwise {
            // Default case for larger sizes
            io.ep_rxData := Cat(
              TloePacGen.toBigEndian(tlPacketVec(3.U +& offset))(15, 0),
              TloePacGen.toBigEndian(tlPacketVec(4.U +& offset))(63, 0)
            )
          }
        }
      }
      io.ep_rxValid := true.B

      tlHandlerState := tlDone   
    }

    is(tlDone) {
      io.ep_rxValid := false.B
      tlHandlerState := tlIdle
    }
  }
} 