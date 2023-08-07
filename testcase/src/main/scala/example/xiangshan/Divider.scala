package example.xiangshan

import chisel3._
import chisel3.util._

class Divider(len: Int = 64) extends Module {
  val io = IO(new Bundle { // 1
    val in = Flipped(new Bundle {
      val ready = Input(Bool())
      val valid = Output(Bool())
      val bits  = Output(Vec(2, Output(UInt(len.W))))
    })
    val sign = Input(Bool())
    val out = new Bundle {
      val ready = Input(Bool())
      val valid = Output(Bool())
      val bits  = Output(UInt((len * 2).W))
    }
  })
  def abs(a: UInt, sign: Bool): (Bool, UInt) = { // 2
    val s = a(len - 1) && sign
    (s, Mux(s, -a, a))
  }

  val s_idle :: s_log2 :: s_shift :: s_compute :: s_finish :: Nil = Enum(5) // 3

  val state  = RegInit(s_idle)                                    // 4
  val newReq = (state === s_idle) && (io.in.ready && io.in.valid) // 5

  val (a, b) = (io.in.bits(0), io.in.bits(1)) // 6
  val divBy0 = b === 0.U(len.W)               // 7

  val shiftReg = Reg(UInt((1 + len * 2).W)) // 8
  val hi       = shiftReg(len * 2, len)     // 9
  val lo       = shiftReg(len - 1, 0)       // 10

  val (aSign, aVal) = abs(a, io.sign)                               // 11
  val (bSign, bVal) = abs(b, io.sign)                               // 12
  val aSignReg      = RegEnable(aSign, newReq)                      // 13
  val qSignReg      = RegEnable((aSign ^ bSign) && !divBy0, newReq) // 14
  val bReg          = RegEnable(bVal, newReq)                       // 15
  val aValx2Reg     = RegEnable(Cat(aVal, "b0".U), newReq)          // 16

  val canSkipShift = (len.U + Log2(bReg)) - Log2(aValx2Reg) // 17
  val enough       = hi.asUInt >= bReg.asUInt               // 18

  val cnt = RegInit(0.U(BigInt(len).bitLength.W)) // 19
  when(newReq) {
    state := s_log2 // 20.1.1
  }.elsewhen(state === s_log2) {
    // here replace `|` to `+`
    cnt   := Mux(divBy0, 0.U, Mux(canSkipShift >= (len - 1).U, (len - 1).U, canSkipShift)) // 20.2.1.1.1
    state := s_shift                                                                       // 20.2.1.1.2
  }.elsewhen(state === s_shift) {
    shiftReg := aValx2Reg << cnt // 20.2.1.2.1.1.1
    state    := s_compute        // 20.2.1.2.1.1.2
  }.elsewhen(state === s_compute) {
    shiftReg := Cat(Mux(enough, hi - bReg, hi)(len - 1, 0), lo, enough) // 20.2.1.2.1.2.1.1.1
    cnt      := cnt + 1.U                                               // 20.2.1.2.1.2.1.1.2
    when(cnt === (len - 1).U) { state := s_finish } // 20.2.1.2.1.2.1.1.3.1.1
  }.elsewhen(state === s_finish) {
    when(io.out.ready) {
      state := s_idle // 20.2.1.2.1.2.1.2.1.1.1.1.1
    }
  }

  val r    = hi(len, 1)             // 21
  val resQ = Mux(qSignReg, -lo, lo) // 22
  val resR = Mux(aSignReg, -r, r)   // 23
  io.out.bits := Cat(resR, resQ) // 24

  io.out.valid := (state === s_finish) // 25
  io.in.ready  := (state === s_idle)   // 26
}
