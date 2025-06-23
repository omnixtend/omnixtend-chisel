package omnixtend

import chisel3._
import chisel3.util._

// 이더넷 헤더 상수를 별도 객체로 분리
object OXFabricEther {
  val srcMac = "h123456789ABC".U
  val destMac = "h001232FFFFFA".U
  val etherType = "hAAAA".U

  // 이더넷 헤더 설정 함수 - 타입 변경
  def populateHeader(ethHeader: EthernetHeader): Unit = {
    ethHeader.destMAC := destMac
    ethHeader.srcMAC := srcMac
    ethHeader.etherType := etherType
  }
}

object OXPacket {

  /** Creates a packet to initiate an OmniXtend open connection.
    *
    * @param seq
    *   Sequence number for the packet.
    * @param chan
    *   Channel ID.
    * @param credit
    *   Credit information for the connection.
    * @return
    *   A UInt representing the full packet with padding.
    */
  def openConnection(seq: UInt, chan: UInt, credit: UInt): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // 이더넷 헤더 필드 설정 - 별도 객체의 메서드 사용
    OXFabricEther.populateHeader(tloePacket.ethHeader)

    // Populate the OmniXtend header fields
    tloePacket.tloeHeader.vc := 0.U // Virtual Channel ID
    // tloePacket.omniHeader.msgType   := Mux(chan === 2.U, 2.U, 0.U)     // Message Type 2 (Open Connection)
    tloePacket.tloeHeader.msgType := Mux(
      chan === 1.U,
      2.U,
      0.U
    ) // Message Type 2 (Open Connection)
    tloePacket.tloeHeader.res1 := 0.U // Reserved field 1
    tloePacket.tloeHeader.seqNum := seq // Sequence Number (0)
    tloePacket.tloeHeader.seqNumAck := "h3FFFFF".U // Acknowledged Sequence Number (2^22-1)
    tloePacket.tloeHeader.ack := 0.U // Acknowledgment flag
    tloePacket.tloeHeader.res2 := 0.U // Reserved field 2
    tloePacket.tloeHeader.chan := chan // Channel ID
    tloePacket.tloeHeader.credit := credit // Credit field

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1 := 0.U // Reserved field 1
    tloePacket.tlMsgHigh.chan := 0.U // Channel ID
    tloePacket.tlMsgHigh.opcode := 0.U // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2 := 0.U // Reserved field 2
    tloePacket.tlMsgHigh.param := 0.U // TileLink parameter field
    tloePacket.tlMsgHigh.size := 0.U // Size of the transaction
    tloePacket.tlMsgHigh.domain := 0.U // Domain field
    tloePacket.tlMsgHigh.err := 0.U // Error field
    tloePacket.tlMsgHigh.res3 := 0.U // Reserved field 3
    tloePacket.tlMsgHigh.source := 0.U // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr := 0.U // TileLink address (input parameter)

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = Cat(tloePacket.asUInt, 0.U(272.W))

    packetWithPadding
  }

  /** Creates a normal acknowledgment (ACK) packet.
    *
    * @param seq
    *   Sequence number for the packet.
    * @param seq_ack
    *   Sequence number being acknowledged.
    * @param ack
    *   Acknowledgment flag.
    * @param chan
    *   Channel ID.
    * @param credit
    *   Updated credit.
    * @return
    *   A UInt representing the full packet with padding.
    */
  def normalAck(seq: UInt, seq_ack: UInt, ack: UInt, chan: UInt, credit: UInt): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // 이더넷 헤더 필드 설정
    OXFabricEther.populateHeader(tloePacket.ethHeader)

    // Populate the OmniXtend header fields
    tloePacket.tloeHeader.vc := 0.U // Virtual Channel ID
    tloePacket.tloeHeader.msgType := 0.U // Message Type 0 (Normal)
    tloePacket.tloeHeader.res1 := 0.U // Reserved field 1
    tloePacket.tloeHeader.seqNum := seq // Sequence Number (0)
    tloePacket.tloeHeader.seqNumAck := seq_ack // Acknowledged Sequence Number (2^22-1)
    tloePacket.tloeHeader.ack := ack // Acknowledgment flag
    tloePacket.tloeHeader.res2 := 0.U // Reserved field 2
    tloePacket.tloeHeader.chan := chan // Channel ID
    tloePacket.tloeHeader.credit := credit // Credit field

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1 := 0.U // Reserved field 1
    tloePacket.tlMsgHigh.chan := 0.U // Channel ID
    tloePacket.tlMsgHigh.opcode := 0.U // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2 := 0.U // Reserved field 2
    tloePacket.tlMsgHigh.param := 0.U // TileLink parameter field
    tloePacket.tlMsgHigh.size := 0.U // Size of the transaction
    tloePacket.tlMsgHigh.domain := 0.U // Domain field
    tloePacket.tlMsgHigh.err := 0.U // Error field
    tloePacket.tlMsgHigh.res3 := 0.U // Reserved field 3
    tloePacket.tlMsgHigh.source := 0.U // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr := 0.U // TileLink address (input parameter)

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = Cat(tloePacket.asUInt, 0.U(272.W))

    packetWithPadding
  }

  def normalAck_896(seq: UInt, seq_ack: UInt, ack: UInt, chan: UInt, credit: UInt): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // 이더넷 헤더 필드 설정
    OXFabricEther.populateHeader(tloePacket.ethHeader)

    // Populate the OmniXtend header fields
    tloePacket.tloeHeader.vc := 0.U // Virtual Channel ID
    tloePacket.tloeHeader.msgType := 0.U // Message Type 0 (Normal)
    tloePacket.tloeHeader.res1 := 0.U // Reserved field 1
    tloePacket.tloeHeader.seqNum := seq // Sequence Number (0)
    tloePacket.tloeHeader.seqNumAck := seq_ack // Acknowledged Sequence Number (2^22-1)
    tloePacket.tloeHeader.ack := ack // Acknowledgment flag
    tloePacket.tloeHeader.res2 := 0.U // Reserved field 2
    tloePacket.tloeHeader.chan := chan // Channel ID
    tloePacket.tloeHeader.credit := credit // Credit field

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1 := 0.U // Reserved field 1
    tloePacket.tlMsgHigh.chan := 0.U // Channel ID
    tloePacket.tlMsgHigh.opcode := 0.U // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2 := 0.U // Reserved field 2
    tloePacket.tlMsgHigh.param := 0.U // TileLink parameter field
    tloePacket.tlMsgHigh.size := 0.U // Size of the transaction
    tloePacket.tlMsgHigh.domain := 0.U // Domain field
    tloePacket.tlMsgHigh.err := 0.U // Error field
    tloePacket.tlMsgHigh.res3 := 0.U // Reserved field 3
    tloePacket.tlMsgHigh.source := 0.U // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr := 0.U // TileLink address (input parameter)

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = Cat(tloePacket.asUInt, 0.U(592.W))

    packetWithPadding
  }

  def ackonly(
      seq: UInt,
      seq_ack: UInt,
      ack: UInt,
      chan: UInt,
      credit: UInt
  ): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // 이더넷 헤더 필드 설정
    OXFabricEther.populateHeader(tloePacket.ethHeader)

    // Populate the OmniXtend header fields
    tloePacket.tloeHeader.vc := 0.U // Virtual Channel ID
    tloePacket.tloeHeader.msgType := 1.U // Message Type 1 (Ack Only)
    tloePacket.tloeHeader.res1 := 0.U // Reserved field 1
    tloePacket.tloeHeader.seqNum := seq // Sequence Number (0)
    tloePacket.tloeHeader.seqNumAck := seq_ack // Acknowledged Sequence Number (2^22-1)
    tloePacket.tloeHeader.ack := ack // Acknowledgment flag
    tloePacket.tloeHeader.res2 := 0.U // Reserved field 2
    tloePacket.tloeHeader.chan := chan // Channel ID
    tloePacket.tloeHeader.credit := credit // Credit field

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1 := 0.U // Reserved field 1
    tloePacket.tlMsgHigh.chan := 0.U // Channel ID
    tloePacket.tlMsgHigh.opcode := 0.U // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2 := 0.U // Reserved field 2
    tloePacket.tlMsgHigh.param := 0.U // TileLink parameter field
    tloePacket.tlMsgHigh.size := 0.U // Size of the transaction
    tloePacket.tlMsgHigh.domain := 0.U // Domain field
    tloePacket.tlMsgHigh.err := 0.U // Error field
    tloePacket.tlMsgHigh.res3 := 0.U // Reserved field 3
    tloePacket.tlMsgHigh.source := 0.U // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr := 0.U // TileLink address (input parameter)

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = Cat(tloePacket.asUInt, 0.U(272.W))

    packetWithPadding
  }

  /** Creates a packet to close an OmniXtend connection.
    *
    * @param seq
    *   Sequence number for the packet.
    * @return
    *   A UInt representing the full packet with padding.
    */
  def closeConnection(seq: UInt): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // 이더넷 헤더 필드 설정
    OXFabricEther.populateHeader(tloePacket.ethHeader)

    // Populate the OmniXtend header fields
    tloePacket.tloeHeader.vc := 0.U // Virtual Channel ID
    tloePacket.tloeHeader.msgType := 3.U // Message Type 3 (Close Connection)
    tloePacket.tloeHeader.res1 := 0.U // Reserved field 1
    tloePacket.tloeHeader.seqNum := seq // Sequence Number (0)
    tloePacket.tloeHeader.seqNumAck := 0.U // Acknowledged Sequence Number (2^22-1)
    tloePacket.tloeHeader.ack := 1.U // Acknowledgment flag
    tloePacket.tloeHeader.res2 := 0.U // Reserved field 2
    tloePacket.tloeHeader.credit := 0.U // Credit field
    tloePacket.tloeHeader.chan := 0.U // Channel ID

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1 := 0.U // Reserved field 1
    tloePacket.tlMsgHigh.chan := 0.U // Channel ID
    tloePacket.tlMsgHigh.opcode := 0.U // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2 := 0.U // Reserved field 2
    tloePacket.tlMsgHigh.param := 0.U // TileLink parameter field
    tloePacket.tlMsgHigh.size := 0.U // Size of the transaction
    tloePacket.tlMsgHigh.domain := 0.U // Domain field
    tloePacket.tlMsgHigh.err := 0.U // Error field
    tloePacket.tlMsgHigh.res3 := 0.U // Reserved field 3
    tloePacket.tlMsgHigh.source := 0.U // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr := 0.U // TileLink address (input parameter)

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = Cat(tloePacket.asUInt, 0.U(272.W))

    packetWithPadding
  }

  /** Creates a packet for a TileLink read operation.
    *
    * @param txAddr
    *   TileLink address.
    * @param seqNum
    *   Sequence number.
    * @param seqNumAck
    *   Acknowledged sequence number.
    * @param size
    *   Transaction size.
    * @return
    *   A UInt representing the full packet with padding.
    */
  def readPacket(txAddr: UInt, seqNum: UInt, seqNumAck: UInt, size: UInt): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // 이더넷 헤더 필드 설정
    OXFabricEther.populateHeader(tloePacket.ethHeader)

    // Populate the OmniXtend header fields
    tloePacket.tloeHeader.vc := 0.U // Virtual Channel ID
    tloePacket.tloeHeader.msgType := 0.U // Message Type 0 (Normal)
    tloePacket.tloeHeader.res1 := 0.U // Reserved field 1
    tloePacket.tloeHeader.seqNum := seqNum // Sequence Number
    tloePacket.tloeHeader.seqNumAck := seqNumAck // Acknowledged Sequence Number
    tloePacket.tloeHeader.ack := 1.U // Acknowledgment flag
    tloePacket.tloeHeader.res2 := 0.U // Reserved field 2
    tloePacket.tloeHeader.chan := 0.U // Channel ID
    tloePacket.tloeHeader.credit := 0.U // Credit field

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1 := 0.U // Reserved field 1
    tloePacket.tlMsgHigh.chan := 1.U // Channel ID
    tloePacket.tlMsgHigh.opcode := 4.U // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2 := 0.U // Reserved field 2
    tloePacket.tlMsgHigh.param := 0.U // TileLink parameter field
    tloePacket.tlMsgHigh.size := size // Size of the transaction
    tloePacket.tlMsgHigh.domain := 0.U // Domain field
    tloePacket.tlMsgHigh.err := 0.U // Error field
    tloePacket.tlMsgHigh.res3 := 0.U // Reserved field 3
    tloePacket.tlMsgHigh.source := 0.U // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr := txAddr // TileLink address (input parameter)
    // tloePacket.tlMsgLow.addr        := "h0000000100000000".U(64.W)

    // Define Padding and Mask
    val padding = 0.U(192.W) // 192-bit padding
    val mask = "h0000000000000001".U(64.W) // 64-bit mask, all bits set to 1

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    val packetWithPadding = Cat(tloePacket.asUInt, padding, mask, 0.U(16.W))

    packetWithPadding
  }

  /** Creates a packet for a TileLink write operation.
    *
    * @param txAddr
    *   TileLink address.
    * @param txData
    *   Data to write.
    * @param seqNum
    *   Sequence number.
    * @param seqNumAck
    *   Acknowledged sequence number.
    * @param size
    *   Transaction size.
    * @return
    *   A UInt representing the full packet with padding.
    */
  def writePacket(txAddr: UInt, txData: UInt, seqNum: UInt, seqNumAck: UInt, size: UInt): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)

    // 이더넷 헤더 필드 설정
    OXFabricEther.populateHeader(tloePacket.ethHeader)

    // Populate the OmniXtend header fields
    tloePacket.tloeHeader.vc := 0.U // Virtual Channel ID
    tloePacket.tloeHeader.msgType := 0.U // Message Type 0 (Normal)
    tloePacket.tloeHeader.res1 := 0.U // Reserved field 1
    tloePacket.tloeHeader.seqNum := seqNum // Sequence Number
    tloePacket.tloeHeader.seqNumAck := seqNumAck // Acknowledged Sequence Number
    tloePacket.tloeHeader.ack := 1.U // Acknowledgment flag
    tloePacket.tloeHeader.res2 := 0.U // Reserved field 2
    tloePacket.tloeHeader.chan := 0.U // Channel ID
    tloePacket.tloeHeader.credit := 0.U // Credit field

    // Populate the high part of the TileLink message fields
    tloePacket.tlMsgHigh.res1 := 0.U // Reserved field 1
    tloePacket.tlMsgHigh.chan := 1.U // Channel ID
    tloePacket.tlMsgHigh.opcode := 0.U // TileLink operation code (input parameter)
    tloePacket.tlMsgHigh.res2 := 0.U // Reserved field 2
    tloePacket.tlMsgHigh.param := 0.U // TileLink parameter field
    tloePacket.tlMsgHigh.size := size // Size of the transaction
    tloePacket.tlMsgHigh.domain := 0.U // Domain field
    tloePacket.tlMsgHigh.err := 0.U // Error field
    tloePacket.tlMsgHigh.res3 := 0.U // Reserved field 3
    tloePacket.tlMsgHigh.source := 0.U // Source field

    // Populate the low part of the TileLink message fields
    tloePacket.tlMsgLow.addr := txAddr // TileLink address (input parameter)

    // Define Padding and Mask
    val mask = "h0000000000000001".U(64.W) // 64-bit mask, all bits set to 1

    // packetWithPadding 정의
    val packetWithPadding = Wire(UInt(576.W))  // 72바이트 = 576비트
    switch(size) {
      is(1.U) {  // 2 Bytes
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(15, 0), 0.U(240.W), mask, 0.U(16.W))
      }
      is(2.U) {  // 4 Bytes
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(31, 0), 0.U(224.W), mask, 0.U(16.W))
      }
      is(3.U) {  // 8 Bytes
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(63, 0), 0.U(192.W), mask, 0.U(16.W))
      }
      is(4.U) {  // 16 Bytes
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(127, 0), 0.U(128.W), mask, 0.U(16.W))
      }
      is(5.U) {  // 32 Bytes
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(255, 0), 0.U(0.W), mask, 0.U(16.W))
      }
      is(6.U) {  // 64 Bytes
        packetWithPadding := Cat(0.U(256.W), tloePacket.asUInt, txData(511, 0), mask, 0.U(16.W))
      }
    }

    packetWithPadding
  }

  def initFrame(txAddr: UInt, txOpcode: UInt, txData: UInt, seqNum: UInt, seqNumAck: UInt, ackType: UInt, char: UInt, credit: UInt, size: UInt, param: UInt, source: UInt): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloePacket = Wire(new TloePacket)
    
    // packetWithPadding 초기화 수정
    val packetWithPadding = WireInit(0.U(896.W))

    // 이더넷 헤더 필드 설정
    OXFabricEther.populateHeader(tloePacket.ethHeader)

    // Populate the OmniXtend header fields
    tloePacket.tloeHeader.vc := 0.U // Virtual Channel ID
    tloePacket.tloeHeader.msgType := 0.U // Message Type 0 (Normal)
    tloePacket.tloeHeader.res1 := 0.U // Reserved field 1
    tloePacket.tloeHeader.seqNum := seqNum // Sequence Number
    tloePacket.tloeHeader.seqNumAck := seqNumAck // Acknowledged Sequence Number
    tloePacket.tloeHeader.ack := ackType // Acknowledgment flag
    tloePacket.tloeHeader.res2 := 0.U // Reserved field 2
    tloePacket.tloeHeader.chan := char // Channel ID
    tloePacket.tloeHeader.credit := credit // Credit field

    // txOpcode 비교 수정
    when(txOpcode === 0.U) {
      // Populate the high part of the TileLink message fields
      tloePacket.tlMsgHigh.res1 := 0.U // Reserved field 1
      tloePacket.tlMsgHigh.chan := 1.U // Channel ID (A)
      tloePacket.tlMsgHigh.opcode := txOpcode // TileLink operation code (input parameter)
      tloePacket.tlMsgHigh.res2 := 0.U // Reserved field 2
      tloePacket.tlMsgHigh.param := param // TileLink parameter field
      tloePacket.tlMsgHigh.size := size // Size of the transaction
      tloePacket.tlMsgHigh.domain := 0.U // Domain field
      tloePacket.tlMsgHigh.err := 0.U // Error field
      tloePacket.tlMsgHigh.res3 := 0.U // Reserved field 3
      tloePacket.tlMsgHigh.source := source // Source field

      // Populate the low part of the TileLink message fields
      tloePacket.tlMsgLow.addr := txAddr
      //tloePacket.tlMsgLow.addr := txData(63, 0)
      //tloePacket.tlMsgLow.addr := txData(511, 448)


      // Define Padding and Mask
      val mask = "h0000000000000001".U(64.W) // 64-bit mask, all bits set to 1

      // packetWithPadding 정의
      switch(size) {
        is(0.U) {  // 0 Bytes
          packetWithPadding := Cat(tloePacket.asUInt, txData(7, 0), 0.U(56.W), 0.U(128.W), mask, 0.U(16.W), 0.U(320.W));
        }
        is(1.U) {  // 2 Bytes
          packetWithPadding := Cat(tloePacket.asUInt, txData(15, 0), 0.U(48.W), 0.U(128.W), mask, 0.U(16.W), 0.U(320.W));
        }
        is(2.U) {  // 4 Bytes
          packetWithPadding := Cat(tloePacket.asUInt, txData(31, 0), 0.U(32.W), 0.U(128.W), mask, 0.U(16.W), 0.U(320.W));
        }
        is(3.U) {  // 8 Bytes
          packetWithPadding := Cat(tloePacket.asUInt, txData(63, 0), 0.U(128.W), mask, 0.U(16.W), 0.U(320.W));
        }
        is(4.U) {  // 16 Bytes
          packetWithPadding := Cat(tloePacket.asUInt, txData(127, 0), 0.U(64.W), mask, 0.U(16.W), 0.U(320.W));
        }
        is(5.U) {  // 32 Bytes
          packetWithPadding := Cat(tloePacket.asUInt, txData(255, 0), mask, 0.U(16.W), 0.U(256.W));
        }
        is(6.U) {  // 64 Bytes
          packetWithPadding := Cat(tloePacket.asUInt, txData(511, 0), mask, 0.U(16.W))
          //packetWithPadding := Cat(tloePacket.asUInt, txData(447, 0), mask, 0.U(64.W), 0.U(16.W))
        }
      }
    }.elsewhen(txOpcode === 4.U) {
      tloePacket.tlMsgHigh.res1 := 0.U // Reserved field 1
      tloePacket.tlMsgHigh.chan := 1.U // Channel ID (A)
      tloePacket.tlMsgHigh.opcode := txOpcode // TileLink operation code (input parameter)
      tloePacket.tlMsgHigh.res2 := 0.U // Reserved field 2
      tloePacket.tlMsgHigh.param := param // TileLink parameter field
      tloePacket.tlMsgHigh.size := size // Size of the transaction
      tloePacket.tlMsgHigh.domain := 0.U // Domain field
      tloePacket.tlMsgHigh.err := 0.U // Error field
      tloePacket.tlMsgHigh.res3 := 0.U // Reserved field 3
      tloePacket.tlMsgHigh.source := source // Source field

      // Populate the low part of the TileLink message fields
      tloePacket.tlMsgLow.addr := txAddr // TileLink address (input parameter)

      // Define Padding and Mask
      val mask = "h0000000000000001".U(64.W) // 64-bit mask, all bits set to 1

      packetWithPadding := Cat(tloePacket.asUInt, 0.U(192.W), mask, 0.U(16.W), 0.U(320.W))
    }.otherwise {
      tloePacket.tlMsgHigh.res1 := 0.U // Reserved field 1
      tloePacket.tlMsgHigh.chan := 0.U // Channel ID (A)
      tloePacket.tlMsgHigh.opcode := 0.U // TileLink operation code (input parameter)
      tloePacket.tlMsgHigh.res2 := 0.U // Reserved field 2
      tloePacket.tlMsgHigh.param := 0.U // TileLink parameter field
      tloePacket.tlMsgHigh.size := 0.U // Size of the transaction
      tloePacket.tlMsgHigh.domain := 0.U // Domain field
      tloePacket.tlMsgHigh.err := 0.U // Error field
      tloePacket.tlMsgHigh.res3 := 0.U // Reserved field 3
      tloePacket.tlMsgHigh.source := 0.U // Source field

      // Populate the low part of the TileLink message fields
      tloePacket.tlMsgLow.addr := 0.U // TileLink address (input parameter)

      // Define Padding and Mask
      val mask = "h0000000000000000".U(64.W) // 64-bit mask, all bits set to 1

      packetWithPadding := Cat(tloePacket.asUInt, 0.U(592.W))
    }
    packetWithPadding
  }
}
