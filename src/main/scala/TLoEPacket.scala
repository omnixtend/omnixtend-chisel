package omnixtend

import chisel3._
import chisel3.util._

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
class OmniXtendHeader extends Bundle {
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
  val omniHeader  = new OmniXtendHeader
  val tlMsgHigh   = new TLMessageHigh
  val tlMsgLow    = new TLMessageLow
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
    val seqNum = Cat(toBigEndian(packet(1))(5, 0), toBigEndian(packet(2))(63, 48))
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
}
