package alu

import chisel3._

class Adder extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(16.W))
    val in2 = Input(UInt(16.W))
    val out = Output(UInt(16.W))
  })

  io.out := io.in1 + io.in2
  val a = 1 / 0;
}
