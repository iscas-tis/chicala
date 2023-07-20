package basic.sort

import chisel3._

/** Split when-oterwise
  */
class Sort2(width: Int) extends Module {
  val io = IO(new Bundle { // 1
    val valid = Input(Bool())
    val in    = Input(UInt(width.W))

    val out = Output(UInt(width.W))
  })

  val a = Wire(UInt(width.W)) // 2
  val b = Wire(UInt(width.W)) // 3
  val c = Wire(UInt(width.W)) // 4

  a := 0.U // 5
  b := a   // 6
  c := 0.U // 7

  when(io.valid) {
    a := io.in // 8.1.1
  }.otherwise {
    c := b // 8.2.1
  }

  io.out := c // 9
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

  a := 0.U
  c := 0.U

  when(io.valid) { a := io.in }
  b := a
  when(io.valid) {}.otherwise { c := b }

  io.out := c
 */
