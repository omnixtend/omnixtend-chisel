package omnixtend

import chisel3._
import chisel3.util._
import chisel3.dontTouch

class TLOEEther extends Module {
  val io = IO(new Bundle {
    // Ethernet Interface
    val rxdata = Input(UInt(64.W))
    val rxvalid = Input(Bool())
    val rxlast = Input(Bool())
    val txdata = Output(UInt(64.W))
    val txvalid = Output(Bool())
    val txlast = Output(Bool())
    val txkeep = Output(UInt(8.W))

    // Packet Interface
    val rxPacketVec = Output(Vec(14, UInt(64.W)))
    val rxPacketVecSize = Output(UInt(4.W))
    val rxPacketEterType = Output(UInt(16.W))
    val rxPacketReceived = Output(Bool())

    val txPacketVec = Input(Vec(14, UInt(64.W)))
    val txPacketVecSize = Input(UInt(4.W))
    val txPacketStart = Input(Bool())
    val txPacketDone = Output(Bool())
  })

  // RX Path Registers
  val rxPacketVec = dontTouch(Reg(Vec(14, UInt(64.W))))
  val rxPacketVecSize = dontTouch(RegInit(0.U(4.W)))
  val rxCount = dontTouch(RegInit(0.U(4.W)))
  val rxPacketEterType = dontTouch(RegInit(0.U(16.W)))
  val rxPacketReceived = dontTouch(RegInit(false.B))

  // TX Path Registers
  val txCount = dontTouch(RegInit(0.U(4.W)))
  val txPacketDone = dontTouch(RegInit(false.B))

  // Initialize outputs
  io.txdata := 0.U
  io.txvalid := false.B
  io.txlast := false.B
  io.txkeep := 0.U
  io.txPacketDone := false.B

  //////////////////////////////////////////////////////////////////
  // RX Path Functions
  def handleRxPath(): Unit = {
    when(!io.rxvalid) {
      rxCount := 0.U
      rxPacketReceived := false.B
    }

    when(io.rxvalid) {
      rxCount := rxCount + 1.U
      rxPacketVec(rxCount) := io.rxdata

      when(io.rxlast) {
        rxPacketVecSize := rxCount + 1.U
        rxPacketEterType := TloePacGen.getEtherType(rxPacketVec)
        rxPacketReceived := true.B
        rxCount := 0.U
      }
    }
  }

  def connectRxOutputs(): Unit = {
    io.rxPacketVec := rxPacketVec
    io.rxPacketVecSize := rxPacketVecSize
    io.rxPacketEterType := rxPacketEterType
    io.rxPacketReceived := rxPacketReceived
  }

  //////////////////////////////////////////////////////////////////
  // TX Path Functions
  def getTxKeep(size: UInt): UInt = {
    // Calculate txkeep based on packet size
    // For 64-bit data, each bit in txkeep corresponds to 8 bits of data
    val txkeep = WireDefault(0xFF.U(8.W))
    switch(size) {
      is(1.U) { txkeep := 0x01.U(8.W) }  // 8 bits
      is(2.U) { txkeep := 0x03.U(8.W) }  // 16 bits
      is(3.U) { txkeep := 0x07.U(8.W) }  // 24 bits
      is(4.U) { txkeep := 0x0F.U(8.W) }  // 32 bits
      is(5.U) { txkeep := 0x1F.U(8.W) }  // 40 bits
      is(6.U) { txkeep := 0x3F.U(8.W) }  // 48 bits
      is(7.U) { txkeep := 0x7F.U(8.W) }  // 56 bits
      is(8.U) { txkeep := 0xFF.U(8.W) }  // 64 bits
    }
    txkeep
  }

  def handleTxPath(): Unit = {
    when(io.txPacketStart) {
      txCount := 0.U
      txPacketDone := false.B
    }

    when(txCount < io.txPacketVecSize) {
      io.txdata := io.txPacketVec(txCount)
      io.txvalid := true.B
      io.txlast := (txCount === (io.txPacketVecSize - 1.U))
      io.txkeep := getTxKeep(io.txPacketVecSize)
      txCount := txCount + 1.U
    }.otherwise {
      io.txvalid := false.B
      io.txlast := false.B
      io.txkeep := 0.U
      txPacketDone := true.B
    }
  }

  def connectTxOutputs(): Unit = {
    io.txPacketDone := txPacketDone
  }

  //////////////////////////////////////////////////////////////////
  // Main Logic
  handleRxPath()
  connectRxOutputs()
  handleTxPath()
  connectTxOutputs()
} 