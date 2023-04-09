package basic.sort

import chisel3._

class Sort1(width: Int) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val in    = Input(UInt(width.W))

    val out = Output(UInt(width.W))
  })

  val a = Wire(UInt(width.W))
  val b = Wire(UInt(width.W))
  val c = Wire(UInt(width.W))
  val d = Wire(UInt(width.W))

  a := 0.U // 1
  b := a   // 2
  c := 0.U // 3
  d := 0.U // 4

  when(io.valid) {
    a := io.in // 5.1
    d := 1.U   // 5.2
    c := b     // 5.3
  }

  io.out := c // 6
}
/* this should be:
  val a = Wire(UInt(width.W))
  val b = Wire(UInt(width.W))
  val c = Wire(UInt(width.W))
  val d = Wire(UInt(width.W))

  a := 0.U
  c := 0.U
  d := 0.U

  When (io.valid) { a := io.in }
  b := a
  When (io.valid) {
    d := 1.U
    c := b
  }

  io.out := c
 */
