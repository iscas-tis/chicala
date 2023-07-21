package basic.sort

import chisel3._

/** Invalid connect couse by last connect semantics
  */
class InvalidConnect1(width: Int) extends Module {
  val io = IO(new Bundle { // 1
    val valid = Input(Bool())
    val in    = Input(UInt(width.W))

    val out = Output(UInt(width.W))
  })

  val a = Wire(UInt(width.W)) // 2
  val b = Wire(UInt(width.W)) // 3
  val c = Wire(UInt(width.W)) // 4

  // here is a invalid connect, overwrite by connect in `when`
  c := io.in // 5

  when(io.valid) {
    c := a // 6.1.1
  }.otherwise {
    c := b // 6.2.1
  }
  a := io.in // 7
  b := io.in // 8

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
