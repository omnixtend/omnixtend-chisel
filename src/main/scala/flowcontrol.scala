package omnixtend

import chisel3._
import chisel3.util._
import chisel3.dontTouch

object Channel {
  val CHANNEL_0 = 0.U
  val CHANNEL_A = 1.U
  val CHANNEL_B = 2.U
  val CHANNEL_C = 3.U
  val CHANNEL_D = 4.U
  val CHANNEL_E = 5.U
}

object CreditInit {
  val INIT_CREDIT = 512  // Initial credit value
  val ERROR_CREDIT = 0xFFFF  // Error value when credit is insufficient
}

class CreditBundle extends Bundle {
  val valid = Input(Bool())
  val channel = Input(UInt(3.W))
  val credit = Input(UInt(16.W))
}

class FlowControl extends Module {
  val io = IO(new Bundle {
    // Credit management signals
    val incCredit = new CreditBundle
    val decCredit = new CreditBundle
    val incAccCredit = new CreditBundle
    val decAccCredit = new CreditBundle

    val credits = Output(Vec(6, UInt(16.W)))  // Current credits for each channel
    val accCredits = Output(Vec(6, UInt(16.W)))  // Accumulated credits
    val maxCreditChannel = Output(UInt(3.W))  // Channel with maximum credit
    val error = Output(Bool())                // Error signal when credit is insufficient
    val maxCredit = Output(UInt(16.W))  // Added for the new maxCredit signal
  })

  // Individual credit registers for each channel
  val CHANNEL_0 = dontTouch(RegInit(CreditInit.INIT_CREDIT.U(16.W)))
  val CHANNEL_A = dontTouch(RegInit(CreditInit.INIT_CREDIT.U(16.W)))
  val CHANNEL_B = dontTouch(RegInit(CreditInit.INIT_CREDIT.U(16.W)))
  val CHANNEL_C = dontTouch(RegInit(CreditInit.INIT_CREDIT.U(16.W)))
  val CHANNEL_D = dontTouch(RegInit(CreditInit.INIT_CREDIT.U(16.W)))
  val CHANNEL_E = dontTouch(RegInit(CreditInit.INIT_CREDIT.U(16.W)))

  // Accumulated credit registers for each channel
  val ACC_CHANNEL_0 = dontTouch(RegInit(0.U(16.W)))
  val ACC_CHANNEL_A = dontTouch(RegInit(0.U(16.W)))
  val ACC_CHANNEL_B = dontTouch(RegInit(0.U(16.W)))
  val ACC_CHANNEL_C = dontTouch(RegInit(0.U(16.W)))
  val ACC_CHANNEL_D = dontTouch(RegInit(0.U(16.W)))
  val ACC_CHANNEL_E = dontTouch(RegInit(0.U(16.W)))

  // Registers to store valid signals
  val incValidReg = RegInit(false.B)
  val decValidReg = RegInit(false.B)

  // Initialize output signals
  io.error := false.B

  // Function to get credit value for a channel
  def getChannelCredit(channel: UInt): UInt = {
    io.credits(channel)
  }

  def getAccChannelCredit(channel: UInt): UInt = {
    io.accCredits(channel)
  }

  // Function to update credit value for a channel
  def updateChannelCredit(channel: UInt, newValue: UInt): Unit = {
    when(channel === Channel.CHANNEL_0) { CHANNEL_0 := newValue }
    .elsewhen(channel === Channel.CHANNEL_A) { CHANNEL_A := newValue }
    .elsewhen(channel === Channel.CHANNEL_B) { CHANNEL_B := newValue }
    .elsewhen(channel === Channel.CHANNEL_C) { CHANNEL_C := newValue }
    .elsewhen(channel === Channel.CHANNEL_D) { CHANNEL_D := newValue }
    .elsewhen(channel === Channel.CHANNEL_E) { CHANNEL_E := newValue }
  }

  // Function to update accumulated credit for a channel
  def updateAccumulatedCredit(channel: UInt, value: UInt): Unit = {
    when(channel === Channel.CHANNEL_0) { ACC_CHANNEL_0 := ACC_CHANNEL_0 + value }
    .elsewhen(channel === Channel.CHANNEL_A) { ACC_CHANNEL_A := ACC_CHANNEL_A + value }
    .elsewhen(channel === Channel.CHANNEL_B) { ACC_CHANNEL_B := ACC_CHANNEL_B + value }
    .elsewhen(channel === Channel.CHANNEL_C) { ACC_CHANNEL_C := ACC_CHANNEL_C + value }
    .elsewhen(channel === Channel.CHANNEL_D) { ACC_CHANNEL_D := ACC_CHANNEL_D + value }
    .elsewhen(channel === Channel.CHANNEL_E) { ACC_CHANNEL_E := ACC_CHANNEL_E + value }
  }

  // Function to find channel with maximum accumulated credit
  def getMaxAccumulatedCreditChannel(): UInt = {
    val accCredits = VecInit(Seq(
      ACC_CHANNEL_0, ACC_CHANNEL_A, ACC_CHANNEL_B,
      ACC_CHANNEL_C, ACC_CHANNEL_D, ACC_CHANNEL_E
    ))
    val maxCredit = accCredits.reduceTree((a, b) => Mux(a >= b, a, b))
    Mux(maxCredit === 0.U, 0.U, PriorityEncoder(accCredits.map(_ === maxCredit)))
  }

  // Function to find the largest power of 2 for a given value
  def getLargestPow(value: UInt): UInt = {
    Mux(value.orR, PriorityEncoder(value), 0.U)
  }

  // Function to get outgoing accumulated credits and decrease the channel's credit
  def getOutgoingAccCredits(channel: UInt): UInt = {
    val currentCredit = getAccChannelCredit(channel)
    // Priority에 0을 넣으면 f 혹은 다른 쓰레기 값이 리턴될수 있듬.
    Mux(currentCredit === 0.U, 0.U, getLargestPow(currentCredit))
  }

  val creditCntA = dontTouch(RegInit(0.U(16.W)))
  val creditCntAAmount = dontTouch(RegInit(0.U(16.W)))
  val creditCntB = dontTouch(RegInit(0.U(16.W)))
  val creditCntBAmount = dontTouch(RegInit(0.U(16.W)))
  val creditCntC = dontTouch(RegInit(0.U(16.W)))
  val creditCntCAmount = dontTouch(RegInit(0.U(16.W)))

  val accValidDebug = dontTouch(RegInit(0.U(1.W)))
  val accChannelDebug = dontTouch(RegInit(0.U(3.W)))
  val accCreditDebug = dontTouch(RegInit(0.U(16.W)))
  accValidDebug := io.decAccCredit.valid
  accChannelDebug := io.decAccCredit.channel
  accCreditDebug := io.decAccCredit.credit


  when(io.incCredit.valid && io.decCredit.valid) {
    val creditIncAmount = io.incCredit.credit
    val creditDecAmount = io.decCredit.credit

    // Create vectors for increment and decrement amounts
    val creditIncVec = VecInit(Seq(
      Mux(io.incCredit.channel === 0.U, creditIncAmount, 0.U),
      Mux(io.incCredit.channel === 1.U, creditIncAmount, 0.U),
      Mux(io.incCredit.channel === 2.U, creditIncAmount, 0.U),
      Mux(io.incCredit.channel === 3.U, creditIncAmount, 0.U),
      Mux(io.incCredit.channel === 4.U, creditIncAmount, 0.U),
      Mux(io.incCredit.channel === 5.U, creditIncAmount, 0.U)
    ))

    val creditDecVec = VecInit(Seq(
      Mux(io.decCredit.channel === 0.U, creditDecAmount, 0.U),
      Mux(io.decCredit.channel === 1.U, creditDecAmount, 0.U),
      Mux(io.decCredit.channel === 2.U, creditDecAmount, 0.U),
      Mux(io.decCredit.channel === 3.U, creditDecAmount, 0.U),
      Mux(io.decCredit.channel === 4.U, creditDecAmount, 0.U),
      Mux(io.decCredit.channel === 5.U, creditDecAmount, 0.U)
    ))

    // Update credits for each channel
    CHANNEL_0 := CHANNEL_0 + creditIncVec(0) - creditDecVec(0)
    CHANNEL_A := CHANNEL_A + creditIncVec(1) - creditDecVec(1)
    CHANNEL_B := CHANNEL_B + creditIncVec(2) - creditDecVec(2)
    CHANNEL_C := CHANNEL_C + creditIncVec(3) - creditDecVec(3)
    CHANNEL_D := CHANNEL_D + creditIncVec(4) - creditDecVec(4)
    CHANNEL_E := CHANNEL_E + creditIncVec(5) - creditDecVec(5)

    creditCntA := creditCntA + 1.U
  }.elsewhen(io.incCredit.valid) {
    val creditAmount = io.incCredit.credit
    // Update regular credit
    switch(io.incCredit.channel) {
      is(0.U) { CHANNEL_0 := CHANNEL_0 + creditAmount }
      is(1.U) { CHANNEL_A := CHANNEL_A + creditAmount }
      is(2.U) { CHANNEL_B := CHANNEL_B + creditAmount }
      is(3.U) { CHANNEL_C := CHANNEL_C + creditAmount }
      is(4.U) { CHANNEL_D := CHANNEL_D + creditAmount }
      is(5.U) { CHANNEL_E := CHANNEL_E + creditAmount }
    }
    creditCntB := creditCntB + 1.U
    creditCntBAmount := creditCntBAmount + creditAmount
  }.elsewhen(io.decCredit.valid) {
    val creditAmount = io.decCredit.credit
    // Update regular credit
    switch(io.decCredit.channel) {
      is(0.U) { CHANNEL_0 := CHANNEL_0 - creditAmount }
      is(1.U) { CHANNEL_A := CHANNEL_A - creditAmount }
      is(2.U) { CHANNEL_B := CHANNEL_B - creditAmount }
      is(3.U) { CHANNEL_C := CHANNEL_C - creditAmount }
      is(4.U) { CHANNEL_D := CHANNEL_D - creditAmount }
      is(5.U) { CHANNEL_E := CHANNEL_E - creditAmount }
    } 
    creditCntC := creditCntC + 1.U
    creditCntCAmount := creditCntCAmount + creditAmount
  }

  val creditAccCntA = dontTouch(RegInit(0.U(16.W)))
  val creditAccCntAAmount = dontTouch(RegInit(0.U(16.W)))
  val creditAccCntB = dontTouch(RegInit(0.U(16.W)))
  val creditAccCntBAmount = dontTouch(RegInit(0.U(16.W)))
  val creditAccCntC = dontTouch(RegInit(0.U(16.W)))
  val creditAccCntCAmount = dontTouch(RegInit(0.U(16.W)))

  when(io.incAccCredit.valid && io.decAccCredit.valid) {
    val creditIncAmount = io.incAccCredit.credit
    val creditDecAmount = io.decAccCredit.credit

    val creditAccIncVec = VecInit(Seq(
      Mux(io.incAccCredit.channel === 0.U, creditIncAmount, 0.U),
      Mux(io.incAccCredit.channel === 1.U, creditIncAmount, 0.U),
      Mux(io.incAccCredit.channel === 2.U, creditIncAmount, 0.U),
      Mux(io.incAccCredit.channel === 3.U, creditIncAmount, 0.U),
      Mux(io.incAccCredit.channel === 4.U, creditIncAmount, 0.U),
      Mux(io.incAccCredit.channel === 5.U, creditIncAmount, 0.U)
    ))

    val creditAccDecVec = VecInit(Seq(
      Mux(io.decAccCredit.channel === 0.U, creditDecAmount, 0.U),
      Mux(io.decAccCredit.channel === 1.U, creditDecAmount, 0.U),
      Mux(io.decAccCredit.channel === 2.U, creditDecAmount, 0.U),
      Mux(io.decAccCredit.channel === 3.U, creditDecAmount, 0.U),
      Mux(io.decAccCredit.channel === 4.U, creditDecAmount, 0.U),
      Mux(io.decAccCredit.channel === 5.U, creditDecAmount, 0.U)
    ))

    ACC_CHANNEL_0 := ACC_CHANNEL_0 + creditAccIncVec(0) - creditAccDecVec(0)
    ACC_CHANNEL_A := ACC_CHANNEL_A + creditAccIncVec(1) - creditAccDecVec(1)
    ACC_CHANNEL_B := ACC_CHANNEL_B + creditAccIncVec(2) - creditAccDecVec(2)
    ACC_CHANNEL_C := ACC_CHANNEL_C + creditAccIncVec(3) - creditAccDecVec(3)
    ACC_CHANNEL_D := ACC_CHANNEL_D + creditAccIncVec(4) - creditAccDecVec(4)
    ACC_CHANNEL_E := ACC_CHANNEL_E + creditAccIncVec(5) - creditAccDecVec(5)

    creditAccCntA := creditAccCntA + 1.U
  }.elsewhen(io.incAccCredit.valid) {
    val creditAmount = io.incAccCredit.credit
    // Update accumulated credit
    switch(io.incAccCredit.channel) {
      is(0.U) { ACC_CHANNEL_0 := ACC_CHANNEL_0 + creditAmount }
      is(1.U) { ACC_CHANNEL_A := ACC_CHANNEL_A + creditAmount }
      is(2.U) { ACC_CHANNEL_B := ACC_CHANNEL_B + creditAmount }
      is(3.U) { ACC_CHANNEL_C := ACC_CHANNEL_C + creditAmount }
      is(4.U) { ACC_CHANNEL_D := ACC_CHANNEL_D + creditAmount }
      is(5.U) { ACC_CHANNEL_E := ACC_CHANNEL_E + creditAmount }
    }
    creditAccCntB := creditAccCntB + 1.U
    creditAccCntBAmount := creditAccCntBAmount + creditAmount
  }.elsewhen(io.decAccCredit.valid) {
    val creditAmount = io.decAccCredit.credit
    // Update accumulated credit
    switch(io.decAccCredit.channel) {
      is(0.U) { ACC_CHANNEL_0 := ACC_CHANNEL_0 - creditAmount }
      is(1.U) { ACC_CHANNEL_A := ACC_CHANNEL_A - creditAmount }
      is(2.U) { ACC_CHANNEL_B := ACC_CHANNEL_B - creditAmount }
      is(3.U) { ACC_CHANNEL_C := ACC_CHANNEL_C - creditAmount }
      is(4.U) { ACC_CHANNEL_D := ACC_CHANNEL_D - creditAmount }
      is(5.U) { ACC_CHANNEL_E := ACC_CHANNEL_E - creditAmount }
    }
    creditAccCntC := creditAccCntC + 1.U
    creditAccCntCAmount := creditAccCntCAmount + creditAmount
  }

  // Register for storing max credit information
  val maxCreditChannelReg = dontTouch(RegInit(0.U(3.W)))
  val maxCreditReg = dontTouch(RegInit(0.U(16.W)))

  // Update max credit information every cycle
  maxCreditChannelReg := getMaxAccumulatedCreditChannel()
  maxCreditReg := getOutgoingAccCredits(maxCreditChannelReg)

  // Function to calculate required credits based on TL message
  def calculateRequiredCredits(opcode: UInt, size: UInt): UInt = {
    val baseCredit = WireDefault(1.U(16.W))
    switch(opcode) {
      is(0.U) { baseCredit := 1.U(16.W) }  // Get
      is(1.U) { baseCredit := 1.U(16.W) }  // PutFullData
      is(2.U) { baseCredit := 1.U(16.W) }  // PutPartialData
      is(3.U) { baseCredit := 1.U(16.W) }  // ArithmeticData
      is(4.U) { baseCredit := 1.U(16.W) }  // LogicalData
      is(5.U) { baseCredit := 1.U(16.W) }  // Intent
      is(6.U) { baseCredit := 1.U(16.W) }  // AcquireBlock
      is(7.U) { baseCredit := 1.U(16.W) }  // AcquirePerm
    }
    baseCredit << size
  }

  // Function to decrease credit based on TL message
  def decreaseCreditByMessage(tlMsg: TLMessageHigh): Bool = {
    val requiredCredit = calculateRequiredCredits(tlMsg.opcode, tlMsg.size)
    val currentCredit = getChannelCredit(tlMsg.chan)
    val hasError = Wire(Bool())
    
    when(currentCredit >= requiredCredit) {
      updateChannelCredit(tlMsg.chan, currentCredit - requiredCredit)
      updateAccumulatedCredit(tlMsg.chan, requiredCredit)
      hasError := false.B
    }.otherwise {
      hasError := true.B
    }
    
    hasError
  }

  // Function to increase credit based on TL message
  def increaseCreditByMessage(tlMsg: TLMessageHigh, credit: UInt): Unit = {
    updateChannelCredit(tlMsg.chan, getChannelCredit(tlMsg.chan) + credit)
    updateAccumulatedCredit(tlMsg.chan, credit)
  }

  // Connect credit values to IO
  io.credits(0) := CHANNEL_0
  io.credits(1) := CHANNEL_A
  io.credits(2) := CHANNEL_B
  io.credits(3) := CHANNEL_C
  io.credits(4) := CHANNEL_D
  io.credits(5) := CHANNEL_E

  // Connect accumulated credit values to IO
  io.accCredits(0) := ACC_CHANNEL_0
  io.accCredits(1) := ACC_CHANNEL_A
  io.accCredits(2) := ACC_CHANNEL_B
  io.accCredits(3) := ACC_CHANNEL_C
  io.accCredits(4) := ACC_CHANNEL_D
  io.accCredits(5) := ACC_CHANNEL_E

  // Connect maxCreditChannel to IO
  io.maxCreditChannel := maxCreditChannelReg

  // maxCredit calculation
  io.maxCredit := maxCreditReg
}
