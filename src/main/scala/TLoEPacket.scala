package omnixtend

import chisel3._
import chisel3.util._

object TLOEHeaderConstants {
  val TLOE_NAK = 0.U(1.W)
  val TLOE_ACK = 1.U(1.W)

  val TLOE_TYPE_NORMAL = 0.U(2.W)
  val TLOE_TYPE_ACKONLY = 1.U(2.W)
  val TLOE_TYPE_OPEN = 2.U(2.W)
  val TLOE_TYPE_CLOSE = 3.U(2.W)
}

/**
 * EthernetHeader class defines the structure of an Ethernet header.
 */
class EthernetHeader extends Bundle {
// val preamble  = UInt(64.W)    // 8-byte Preamble/SFD
  val destMAC   = UInt(48.W)    // 6-byte Destination MAC Address
  val srcMAC    = UInt(48.W)    // 6-byte Source MAC Address
  val etherType = UInt(16.W)    // 2-byte EtherType field
}

/**
 * OmniXtendHeader class defines the structure of an OmniXtend header.
 * 64 Bits (8 Bytes)
 */
class tloeHeader extends Bundle {
  val vc        = UInt(3.W)     // Virtual Channel
  val msgType   = UInt(4.W)     // Reserved
  val res1      = UInt(3.W)     // Reserved
  val seqNum    = UInt(22.W)    // Sequence Number
  val seqNumAck = UInt(22.W)    // Sequence Number Acknowledgment
  val ack       = UInt(1.W)     // Acknowledgment
  val res2      = UInt(1.W)     // Reserved
  val chan      = UInt(3.W)     // Channel
  val credit    = UInt(5.W)     // Credit
}

/**
 * TileLinkMessage class defines the structure of a TileLink message.
 * 64 Bits (8 Bytes)
 */
class TLMessageHigh extends Bundle {
  val res1      = UInt(1.W)     // Reserved
  val chan      = UInt(3.W)     // Channel
  val opcode    = UInt(3.W)     // Opcode
  val res2      = UInt(1.W)     // Reserved
  val param     = UInt(4.W)     // Parameter
  val size      = UInt(4.W)     // Size
  val domain    = UInt(8.W)     // Domain
  val err       = UInt(2.W)     // Error
  val res3      = UInt(12.W)    // Reserved
  val source    = UInt(26.W)    // Source
}

/**
 * TileLinkMessage class defines the structure of a TileLink message.
 */
class TLMessageLow extends Bundle {
  val addr      = UInt(64.W)    // Address
}

/**
 * TloePacket class defines the structure of a TLoE packet.
 */
class TloePacket extends Bundle {
  val ethHeader   = new EthernetHeader
  val tloeHeader  = new tloeHeader
  val tlMsgHigh   = new TLMessageHigh
  val tlMsgLow    = new TLMessageLow
}

object MsgType {
  val NORMAL     = 0.U(4.W)     // Normal message
  val ACKONLY    = 1.U(4.W)     // Acknowledgment only message
  val OPECONN    = 2.U(4.W)     // Open connection message
  val CLOSECONN  = 3.U(4.W)     // Close connection message
}

/**
 * TloePacketGenerator object contains functions to create and manipulate TLoE packets.
 */
object TloePacGen {

  /**
   * Converts a 64-bit unsigned integer from little-endian to big-endian format.
   * @param value A 64-bit UInt to be converted.
   * @return A 64-bit UInt in big-endian format.
   */
  def toBigEndian(value: UInt): UInt = {
    require(value.getWidth == 64, "Input must be 64 bits wide")  // Ensure the input is 64 bits wide

    // Rearrange the bytes of the input value to convert it to big-endian format
    Cat(
      value(7, 0),     // Least significant byte (original bits 7:0)
      value(15, 8),    // Next byte (original bits 15:8)
      value(23, 16),   // Next byte (original bits 23:16)
      value(31, 24),   // Next byte (original bits 31:24)
      value(39, 32),   // Next byte (original bits 39:32)
      value(47, 40),   // Next byte (original bits 47:40)
      value(55, 48),   // Next byte (original bits 55:48)
      value(63, 56)    // Most significant byte (original bits 63:56)
    )
  }

  /**
   * Extracts the EtherType field from a packet.
   * @param packet A vector of UInts representing the packet.
   * @return The EtherType field as a 16-bit UInt.
   */
  def getEtherType(packet: Vec[UInt]): UInt = {
    val seqNum = toBigEndian(packet(1))(31, 16)
    seqNum
  }

  /**
   * Extracts the Sequence Number field from a packet.
   * @param packet A vector of UInts representing the packet.
   * @return The Sequence Number as a 22-bit UInt.
   */
  def getSeqNum(packet: Vec[UInt]): UInt = {
    val seqNum = Cat((toBigEndian(packet(1)))(5, 0), (toBigEndian(packet(2)))(63, 48))
    seqNum
  }

  def getSeqNumNoEndian(packet: Vec[UInt]): UInt = {
    val seqNum = Cat(packet(1)(5, 0), packet(2)(63, 48))
    seqNum
  }

  /**
   * Extracts the Sequence Number Acknowledgment field from a packet.
   * @param packet A vector of UInts representing the packet.
   * @return The Sequence Number Acknowledgment as a 22-bit UInt.
   */
  def getSeqNumAck(packet: Vec[UInt]): UInt = {
    val seqNumAck = toBigEndian(packet(2))(47, 26)  // Extracts bits 23:21 from packet(2)
    seqNumAck
  }

  def getMsgType(packet: Vec[UInt]): UInt = {
    val msgType = toBigEndian(packet(1))(12, 9)
    msgType
  }

  /**
   * Extracts the Channel ID from a packet.
   * @param packet A vector of UInts representing the packet.
   * @return The Channel ID as a 3-bit UInt.
   */
  def getChan(packet: Vec[UInt]): UInt = {
    val chan = toBigEndian(packet(2))(23, 21)  // Extracts bits 23:21 from packet(2)
    chan
  }

  /**
   * Extracts the Credit field from a packet.
   * @param packet A vector of UInts representing the packet.
   * @return The Credit field as a 5-bit UInt.
   */
  def getCredit(packet: Vec[UInt]): UInt = {
    val credit = toBigEndian(packet(2))(20, 16)  // Extracts bits 20:16 from packet(2) 
    credit
  }

  /**
   * Extracts a Mask field from a packet.
   * The mask is used to define which parts of the packet are valid.
   * @param packet A vector of UInts representing the packet.
   * @param size The number of 64-bit elements in the packet.
   * @return The mask as a 64-bit UInt.
   */
  def getMask(packet: Vec[UInt], size: UInt): UInt = {
    val mask = Cat(toBigEndian(packet(size-2.U))(15, 0), toBigEndian(packet(size-1.U))(63, 16))
    mask
  }

  def getType(packet: Vec[UInt]): UInt = {
    // msgType is in the first 64-bit word (packet(0)) at bits 63-60
    val msgType = toBigEndian(packet(1))(12, 9)
    msgType
  }

  def getAck(packet: Vec[UInt]): UInt = {
    val ack = toBigEndian(packet(2))(25, 25)
    ack
  }

  // 패킷 크기 계산 함수
  def getPacketSize(size: UInt): UInt = {
    // size는 2의 거듭제곱으로 표현된 데이터 크기 (바이트 단위)
    // 예: size=3이면 8바이트 (2^3)
    // 패킷 크기는 64비트(8바이트) 단위로 계산
    // 헤더(8바이트) + 데이터(2^size 바이트) + 마스크(1바이트) + 패딩
    val dataSize = 1.U << size
    val totalSize = (dataSize + 9.U + 7.U) >> 3.U  // (데이터 + 헤더+마스크 + 7) / 8 (올림)
    totalSize
  }
}
