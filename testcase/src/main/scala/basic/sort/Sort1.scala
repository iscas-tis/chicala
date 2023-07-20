package basic.sort

import chisel3._

/** Split when body
  */
class Sort1(width: Int) extends Module {
  val io = IO(new Bundle { // 1
    val valid = Input(Bool())
    val in    = Input(UInt(width.W))

    val out = Output(UInt(width.W))
  })

  val a = Wire(UInt(width.W)) // 2
  val b = Wire(UInt(width.W)) // 3
  val c = Wire(UInt(width.W)) // 4
  val d = Wire(UInt(width.W)) // 5

  a := 0.U // 6
  b := a   // 7
  c := 0.U // 8
  d := 0.U // 9

  when(io.valid) {
    a := io.in // 10.1.1
    d := 1.U   // 10.1.2
    c := b     // 10.1.3
  }

  io.out := c // 11
}
/* this should be:
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val in    = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })

  val a = Wire(UInt(width.W))
  val b = Wire(UInt(width.W))
  val c = Wire(UInt(width.W))
  val d = Wire(UInt(width.W))

  a := 0.U
  c := 0.U
  d := 0.U

  When (io.valid) {
    a := io.in
    d := 1.U
  }
  b := a
  When (io.valid) { c := b }

  io.out := c
 */
