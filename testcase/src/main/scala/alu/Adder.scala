package alu

import chisel3._

class Adder(width: Int) extends Module {
  val io = IO(new Bundle { // 1
    val valid = Input(Bool())
    val in1   = Input(UInt(width.W))
    val in2   = Input(UInt(width.W))
    val out   = Output(UInt(width.W))
  })

  when(io.valid) {
    io.out := io.in1 + io.in2 // 2.1.1
  } otherwise {
    io.out := 0.U // 2.2.1
  }

  assert((io.valid && io.out === io.in1 + io.in2) || (!io.valid && io.out === 0.U)) // 3
}
