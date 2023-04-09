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
  c := io.in

  when(io.valid) {
    c := a
  }.otherwise {
    c := b
  }
  a := io.in
  b := io.in

  io.out := c
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
