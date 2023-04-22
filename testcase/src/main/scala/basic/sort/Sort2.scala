package basic.sort

import chisel3._

/** Split when-oterwise
  */
class Sort2(width: Int) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val in    = Input(UInt(width.W))

    val out = Output(UInt(width.W))
  })

  val a = Wire(UInt(width.W))
  val b = Wire(UInt(width.W))
  val c = Wire(UInt(width.W))

  a := 0.U // 1
  b := a   // 2
  c := 0.U // 3

  when(io.valid) {
    a := io.in // 4.1.1
  }.otherwise {
    c := b // 4.2.1
  }

  io.out := c // 5
}
/* this should be:
  val a = Wire(UInt(width.W))
  val b = Wire(UInt(width.W))
  val c = Wire(UInt(width.W))

  a := 0.U // 1
  c := 0.U // 3

  when(io.valid) {
    a := io.in // 4.1.1
  }
  b := a   // 2
  when(io.valid) {}.otherwise {
    c := b // 4.2.1
  }

  io.out := c // 5
 */
