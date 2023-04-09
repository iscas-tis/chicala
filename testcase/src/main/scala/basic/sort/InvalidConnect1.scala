package basic.sort

import chisel3._

class InvalidConnect1(width: Int) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val in    = Input(UInt(width.W))

    val out = Output(UInt(width.W))
  })

  val a = Wire(UInt(width.W))
  val b = Wire(UInt(width.W))
  val c = Wire(UInt(width.W))

  // here is a invalid connect, overwrite by connect in `when`
  c := io.in // 1

  when(io.valid) {
    c := a // 2.1.1
  }.otherwise {
    c := b // 2.2.1
  }
  a := io.in // 3
  b := io.in // 4

  io.out := c // 5
}
/* this should be:
  val a = Wire(UInt(width.W))
  val b = Wire(UInt(width.W))
  val c = Wire(UInt(width.W))

  c := io.in

  a := io.in
  b := io.in

  when(io.valid) {
    c := a
  }.otherwise {
    c := b
  }

  io.out := c
 */
