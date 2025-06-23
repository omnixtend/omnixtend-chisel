package omnixtend

import chisel3._
import chisel3.util._

/**
 * Endpoint module handles memory read and write operations based on the received
 * TileLink messages. It serializes and deserializes data to/from the queues.
 * This version uses a 1MB memory and applies an address offset of 0x100000000.
 */
class Endpoint extends Module {
  val io = IO(new Bundle {
    //val txQueueData = Flipped(Decoupled(new TloePacket)) // Input data from the transceiver, TloePacket type
    //val rxQueueData = Decoupled(new TloePacket)          // Output data to the transceiver, TloePacket type
    val txQueueData = Flipped(Decoupled(UInt(131.W))) // Input data from the transceiver, TloePacket type
    val rxQueueData = Decoupled(UInt(131.W))          // Output data to the transceiver, TloePacket type
  })

  // Default values for I/O signals
  io.txQueueData.ready := false.B
  io.rxQueueData.valid := false.B
  //io.rxQueueData.bits  := 0.U.asTypeOf(new TloePacket)
  io.rxQueueData.bits  := 0.U

  // 1MB memory for read and write operations
  // 131072 entries of 64-bit data (1MB total)
  val mem = SyncReadMem(131072, UInt(64.W))

  // Address offset value
  // All addresses will have this offset subtracted to map to memory
  val addressOffset = 0x100000000L.U(64.W)

  // Handle data from txQueueData
  when(io.txQueueData.valid) {
    // Extract address, data, and opcode from the input TloePacket
    val txPacket = io.txQueueData.bits
    //val addr     = txPacket.tileLinkMsg.addr - addressOffset // 64 bits for address, subtract offset
    //val data     = txPacket.tileLinkMsg.data                 // 64 bits for data
    //val opcode   = txPacket.tileLinkMsg.opcode               // 3 bits for opcode
    val addr     = txPacket(127,64) - addressOffset // 64 bits for address, subtract offset
    val data     = txPacket(63,0)                 // 64 bits for data
    val opcode   = txPacket(66,64)               // 3 bits for opcode

    // Perform operations based on the opcode
    switch(opcode) {
      is(4.U) { // GET operation
        // Read data from memory at the specified address
        val readData = mem.read(addr)
        // Combine the address, read data, and opcode into the TileLinkMessage part of rxQueueData
//        io.rxQueueData.bits.tileLinkMsg.addr   := addr + addressOffset
//        io.rxQueueData.bits.tileLinkMsg.data   := readData
//        io.rxQueueData.bits.tileLinkMsg.opcode := opcode
        io.rxQueueData.bits := 123456.U
      }
      is(0.U) { // PutFullData operation
        // Write data to memory at the specified address
        mem.write(addr, data)
        // Combine the address, written data, and opcode into the TileLinkMessage part of rxQueueData
//        io.rxQueueData.bits.tileLinkMsg.addr   := addr + addressOffset
//        io.rxQueueData.bits.tileLinkMsg.data   := data
//        io.rxQueueData.bits.tileLinkMsg.opcode := opcode
        io.rxQueueData.bits := 567890.U
      }
    }

    // Pass through Ethernet and OmniXtend headers from the input TloePacket to the output TloePacket
    //io.rxQueueData.bits.ethHeader  := txPacket.ethHeader
    //io.rxQueueData.bits.omniHeader := txPacket.omniHeader
    //io.rxQueueData.bits.padding    := txPacket.padding
    //io.rxQueueData.bits.tloeMask   := txPacket.tloeMask
    io.rxQueueData.bits := 0.U

    // Indicate that the output data is valid and the Endpoint is ready to receive new data
    io.rxQueueData.valid := true.B
    io.txQueueData.ready := true.B
  }
}

