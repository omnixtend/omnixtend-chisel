package omnixtend

import chisel3._
import chisel3.util._

object GlobalTimerConstants {
  val TIMEOUT_THRESHOLD = 1000000000.U(64.W) // 10s at 100MHz clock
}

/** GlobalTimer module provides a global timer for the entire system.
  * This timer is used for tracking packet transmission times and timeout management.
  */
class GlobalTimer extends Module {
  import GlobalTimerConstants._

  val io = IO(new Bundle {
    val globalTimer = Output(UInt(64.W))  // Global timer output
    val resetTimer = Input(Bool())         // Reset timer signal
  })

  // 64-bit timer register
  val timer = dontTouch(RegInit(0.U(64.W)))

  // Reset timer when resetTimer is asserted
  when(io.resetTimer) {
    timer := 0.U
  }.otherwise {
    timer := timer + 1.U
  }

  // Connect timer to output
  io.globalTimer := timer
}

object Timer {
  import GlobalTimerConstants._

  // Helper function to calculate time difference considering wrap-around
  def timeDiff(current: UInt, previous: UInt): UInt = {
    Mux(current >= previous,
        current - previous,
        (current + (1.U << 64)) - previous)
  }

  // Function to check if timeout has occurred
  def isTimeout(current: UInt, refTime: UInt): Bool = {
    timeDiff(current, refTime) >= TIMEOUT_THRESHOLD
  }
} 