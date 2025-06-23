package omnixtend

import chisel3._
import chisel3.util._

object OXPacket {
  val srcMac    = "h123456789ABC".U
  val destMac   = "h001232FFFFFA".U
  val etherType = "hAAAA".U 

  /**
   * Creates a packet to initiate an OmniXtend open connection.
   *
   * @param seq Sequence number for the packet.
   * @param chan Channel ID.
   * @param credit Credit information for the connection.
   * @return A UInt representing the full packet with padding.
   */
  def openConnection(seq:UInt, chan: UInt, credit: UInt): UInt ={
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // Populate the Ethernet header fields
    tloePacket.ethHeader.destMAC    := destMac   // 6-byte Destination MAC Address
    tloePacket.ethHeader.srcMAC     := srcMac    // 6-byte Source MAC Address
    tloePacket.ethHeader.etherType  := etherType // 2-byte EtherType (Example value for TLoE)

    // Populate the OmniXtend header fields
    tloePacket.omniHeader.vc        := 0.U      // Virtual Channel ID
    //tloePacket.omniHeader.msgType   := Mux(chan === 2.U, 2.U, 0.U)     // Message Type 2 (Open Connection)
    tloePacket.omniHeader.msgType   := Mux(chan === 1.U, 2.U, 0.U)     // Message Type 2 (Open Connection)
    tloePacket.omniHeader.res1      := 0.U      // Reserved field 1
    tloePacket.omniHeader.seqNum    := seq      // Sequence Number (0)
    tloePacket.omniHeader.seqNumAck := "h3FFFFF".U  // Acknowledged Sequence Number (2^22-1)
    tloePacket.omniHeader.ack       := 0.U      // Acknowledgment flag
    tloePacket.omniHeader.res2      := 0.U      // Reserved field 2
    tloePacket.omniHeader.chan      := chan     // Channel ID
    tloePacket.omniHeader.credit    := credit   // Credit field

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1       := 0.U      // Reserved field 1
    tloePacket.tlMsgHigh.chan       := 0.U      // Channel ID
    tloePacket.tlMsgHigh.opcode     := 0.U      // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2       := 0.U      // Reserved field 2
    tloePacket.tlMsgHigh.param      := 0.U      // TileLink parameter field
    tloePacket.tlMsgHigh.size       := 0.U      // Size of the transaction
    tloePacket.tlMsgHigh.domain     := 0.U      // Domain field
    tloePacket.tlMsgHigh.err        := 0.U      // Error field
    tloePacket.tlMsgHigh.res3       := 0.U      // Reserved field 3
    tloePacket.tlMsgHigh.source     := 0.U      // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr        := 0.U      // TileLink address (input parameter)

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = Cat(tloePacket.asUInt, 0.U(272.W))
    
    packetWithPadding
  }

  /**
   * Creates a normal acknowledgment (ACK) packet.
   *
   * @param seq Sequence number for the packet.
   * @param seq_ack Sequence number being acknowledged.
   * @param ack Acknowledgment flag.
   * @param chan Channel ID.
   * @param credit Updated credit.
   * @return A UInt representing the full packet with padding.
   */
  def normalAck(seq:UInt, seq_ack:UInt, ack:UInt, chan:UInt, credit:UInt): UInt ={
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // Populate the Ethernet header fields
    tloePacket.ethHeader.destMAC    := destMac   // 6-byte Destination MAC Address
    tloePacket.ethHeader.srcMAC     := srcMac    // 6-byte Source MAC Address
    tloePacket.ethHeader.etherType  := etherType // 2-byte EtherType (Example value for TLoE)

    // Populate the OmniXtend header fields
    tloePacket.omniHeader.vc        := 0.U      // Virtual Channel ID
    tloePacket.omniHeader.msgType   := 0.U      // Message Type 0 (Normal)  
    tloePacket.omniHeader.res1      := 0.U      // Reserved field 1
    tloePacket.omniHeader.seqNum    := seq      // Sequence Number (0)
    tloePacket.omniHeader.seqNumAck := seq_ack  // Acknowledged Sequence Number (2^22-1)
    tloePacket.omniHeader.ack       := ack      // Acknowledgment flag
    tloePacket.omniHeader.res2      := 0.U      // Reserved field 2
    tloePacket.omniHeader.chan      := chan     // Channel ID
    tloePacket.omniHeader.credit    := credit   // Credit field

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1       := 0.U      // Reserved field 1
    tloePacket.tlMsgHigh.chan       := 0.U      // Channel ID
    tloePacket.tlMsgHigh.opcode     := 0.U      // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2       := 0.U      // Reserved field 2
    tloePacket.tlMsgHigh.param      := 0.U      // TileLink parameter field
    tloePacket.tlMsgHigh.size       := 0.U      // Size of the transaction
    tloePacket.tlMsgHigh.domain     := 0.U      // Domain field
    tloePacket.tlMsgHigh.err        := 0.U      // Error field
    tloePacket.tlMsgHigh.res3       := 0.U      // Reserved field 3
    tloePacket.tlMsgHigh.source     := 0.U      // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr        := 0.U      // TileLink address (input parameter)

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = Cat(tloePacket.asUInt, 0.U(272.W))
    
    packetWithPadding
  }

  /**
   * Creates a packet to close an OmniXtend connection.
   *
   * @param seq Sequence number for the packet.
   * @return A UInt representing the full packet with padding.
   */
  def closeConnection(seq:UInt): UInt ={
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // Populate the Ethernet header fields
    tloePacket.ethHeader.destMAC    := destMac   // 6-byte Destination MAC Address
    tloePacket.ethHeader.srcMAC     := srcMac    // 6-byte Source MAC Address
    tloePacket.ethHeader.etherType  := etherType // 2-byte EtherType (Example value for TLoE)

    // Populate the OmniXtend header fields
    tloePacket.omniHeader.vc        := 0.U      // Virtual Channel ID
    tloePacket.omniHeader.msgType   := 3.U      // Message Type 3 (Close Connection)
    tloePacket.omniHeader.res1      := 0.U      // Reserved field 1
    tloePacket.omniHeader.seqNum    := seq      // Sequence Number (0)
    tloePacket.omniHeader.seqNumAck := 0.U      // Acknowledged Sequence Number (2^22-1)
    tloePacket.omniHeader.ack       := 1.U      // Acknowledgment flag
    tloePacket.omniHeader.res2      := 0.U      // Reserved field 2
    tloePacket.omniHeader.credit    := 0.U      // Credit field
    tloePacket.omniHeader.chan      := 0.U      // Channel ID

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1       := 0.U      // Reserved field 1
    tloePacket.tlMsgHigh.chan       := 0.U      // Channel ID
    tloePacket.tlMsgHigh.opcode     := 0.U      // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2       := 0.U      // Reserved field 2
    tloePacket.tlMsgHigh.param      := 0.U      // TileLink parameter field
    tloePacket.tlMsgHigh.size       := 0.U      // Size of the transaction
    tloePacket.tlMsgHigh.domain     := 0.U      // Domain field
    tloePacket.tlMsgHigh.err        := 0.U      // Error field
    tloePacket.tlMsgHigh.res3       := 0.U      // Reserved field 3
    tloePacket.tlMsgHigh.source     := 0.U      // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr        := 0.U      // TileLink address (input parameter)

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = Cat(tloePacket.asUInt, 0.U(272.W))
    
    packetWithPadding
  }

  /**
   * Creates a packet for a TileLink read operation.
   *
   * @param txAddr TileLink address.
   * @param seqNum Sequence number.
   * @param seqNumAck Acknowledged sequence number.
   * @param size Transaction size.
   * @return A UInt representing the full packet with padding.
   */
  def readPacket(txAddr: UInt, seqNum: UInt, seqNumAck: UInt, size: UInt): UInt ={

    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // Populate the Ethernet header fields
    tloePacket.ethHeader.destMAC    := destMac   // 6-byte Destination MAC Address
    tloePacket.ethHeader.srcMAC     := srcMac    // 6-byte Source MAC Address
    tloePacket.ethHeader.etherType  := etherType // 2-byte EtherType (Example value for TLoE)

    // Populate the OmniXtend header fields
    tloePacket.omniHeader.vc        := 0.U      // Virtual Channel ID
    tloePacket.omniHeader.msgType   := 0.U      // Message Type 0 (Normal)  
    tloePacket.omniHeader.res1      := 0.U      // Reserved field 1
    tloePacket.omniHeader.seqNum    := seqNum   // Sequence Number
    tloePacket.omniHeader.seqNumAck := seqNumAck// Acknowledged Sequence Number
    tloePacket.omniHeader.ack       := 1.U      // Acknowledgment flag
    tloePacket.omniHeader.res2      := 0.U      // Reserved field 2
    tloePacket.omniHeader.chan      := 0.U      // Channel ID
    tloePacket.omniHeader.credit    := 0.U      // Credit field

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1       := 0.U      // Reserved field 1
    tloePacket.tlMsgHigh.chan       := 1.U      // Channel ID
    tloePacket.tlMsgHigh.opcode     := 4.U      // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2       := 0.U      // Reserved field 2
    tloePacket.tlMsgHigh.param      := 0.U      // TileLink parameter field
    tloePacket.tlMsgHigh.size       := size     // Size of the transaction
    tloePacket.tlMsgHigh.domain     := 0.U      // Domain field
    tloePacket.tlMsgHigh.err        := 0.U      // Error field
    tloePacket.tlMsgHigh.res3       := 0.U      // Reserved field 3
    tloePacket.tlMsgHigh.source     := 0.U      // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr        := txAddr  // TileLink address (input parameter)
    //tloePacket.tlMsgLow.addr        := "h0000000100000000".U(64.W)

    // Define Padding and Mask
    val padding = 0.U(192.W)                    // 192-bit padding
    val mask = "h0000000000000001".U(64.W)      // 64-bit mask, all bits set to 1

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = Cat(tloePacket.asUInt, padding, mask, 0.U(16.W))
    
    packetWithPadding
  }

  /**
   * Creates a packet for a TileLink write operation.
   *
   * @param txAddr TileLink address.
   * @param txData Data to write.
   * @param seqNum Sequence number.
   * @param seqNumAck Acknowledged sequence number.
   * @param size Transaction size.
   * @return A UInt representing the full packet with padding.
   */
  def writePacket(txAddr: UInt, txData:UInt, seqNum: UInt, seqNumAck: UInt, size: UInt): UInt ={
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // Populate the Ethernet header fields
    tloePacket.ethHeader.destMAC    := destMac   // 6-byte Destination MAC Address
    tloePacket.ethHeader.srcMAC     := srcMac    // 6-byte Source MAC Address
    tloePacket.ethHeader.etherType  := etherType // 2-byte EtherType (Example value for TLoE)

    // Populate the OmniXtend header fields
    tloePacket.omniHeader.vc        := 0.U      // Virtual Channel ID
    tloePacket.omniHeader.msgType   := 0.U      // Message Type 0 (Normal)  
    tloePacket.omniHeader.res1      := 0.U      // Reserved field 1
    tloePacket.omniHeader.seqNum    := seqNum   // Sequence Number
    tloePacket.omniHeader.seqNumAck := seqNumAck// Acknowledged Sequence Number
    tloePacket.omniHeader.ack       := 1.U      // Acknowledgment flag
    tloePacket.omniHeader.res2      := 0.U      // Reserved field 2
    tloePacket.omniHeader.chan      := 0.U      // Channel ID
    tloePacket.omniHeader.credit    := 0.U      // Credit field

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1       := 0.U      // Reserved field 1
    tloePacket.tlMsgHigh.chan       := 1.U      // Channel ID
    tloePacket.tlMsgHigh.opcode     := 0.U      // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2       := 0.U      // Reserved field 2
    tloePacket.tlMsgHigh.param      := 0.U      // TileLink parameter field
    tloePacket.tlMsgHigh.size       := size     // Size of the transaction
    tloePacket.tlMsgHigh.domain     := 0.U      // Domain field
    tloePacket.tlMsgHigh.err        := 0.U      // Error field
    tloePacket.tlMsgHigh.res3       := 0.U      // Reserved field 3
    tloePacket.tlMsgHigh.source     := 0.U      // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr        := txAddr  // TileLink address (input parameter)

    // Define Padding and Mask
    val mask = "h0000000000000001".U(64.W)      // 64-bit mask, all bits set to 1

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = WireInit(UInt(896.W), 0.U)

    switch(size) {
      is(1.U) {   // 2 Bytes (2^1)
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(15, 0), 0.U(240.W), mask, 0.U(16.W))
      }
      is(2.U) {   // 4 Bytes (2^2)
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(31, 0), 0.U(224.W), mask, 0.U(16.W))
      } 
      is(3.U) {   // 8 Bytes (2^3)
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(63, 0), 0.U(192.W), mask, 0.U(16.W))
      }
      is(4.U) {   // 16 Bytes (2^4)
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(127, 0), 0.U(128.W), mask, 0.U(16.W))
      }
      is(5.U) {   // 32 Bytes (2^5) 
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(255, 0), mask, 0.U(16.W))
      }
      is(6.U) {   // 64 Bytes (2^6)
        packetWithPadding := Cat(tloePacket.asUInt, txData(511, 0), mask, 0.U(16.W))
      }
    }
    
    packetWithPadding
  }
}
