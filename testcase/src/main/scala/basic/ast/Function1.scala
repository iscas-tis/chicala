package basic.ast

import chisel3._
import chisel3.util._

class Function1(width1: Int, width2: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(width1.W))
    val out = Output(UInt(width2.W))
  })

  def signExt(a: UInt, width: Int): UInt = {
    Cat(Fill(width - width1, a(width1 - 1)), a)
  }

  io.out := signExt(io.in, width2)
}
