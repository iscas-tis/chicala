package alu

import chisel3._

class Adder(width: Int) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val in1   = Input(UInt(width.W))
    val in2   = Input(UInt(width.W))
    val out   = Output(UInt(width.W))
  })

  when(io.valid) {
    io.out := io.in1 + io.in2
  } otherwise {
    io.out := 0.U
  }
}
