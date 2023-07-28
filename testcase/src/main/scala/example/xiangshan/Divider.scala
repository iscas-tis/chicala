package example.xiangshan

import chisel3._
import chisel3.util._

class Divider(len: Int = 64) extends Module {
  val io = IO(new Bundle {
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
  def abs(a: UInt, sign: Bool): (Bool, UInt) = {
    val s = a(len - 1) && sign
    (s, Mux(s, -a, a))
  }

  val s_idle :: s_log2 :: s_shift :: s_compute :: s_finish :: Nil = Enum(5)

  val state  = RegInit(s_idle)
  val newReq = (state === s_idle) && (io.in.ready && io.in.valid)

  val (a, b) = (io.in.bits(0), io.in.bits(1))
  val divBy0 = b === 0.U(len.W)

  val shiftReg = Reg(UInt((1 + len * 2).W))
  val hi       = shiftReg(len * 2, len)
  val lo       = shiftReg(len - 1, 0)

  val (aSign, aVal) = abs(a, io.sign)
  val (bSign, bVal) = abs(b, io.sign)
  val aSignReg      = RegEnable(aSign, newReq)
  val qSignReg      = RegEnable((aSign ^ bSign) && !divBy0, newReq)
  val bReg          = RegEnable(bVal, newReq)
  val aValx2Reg     = RegEnable(Cat(aVal, "b0".U), newReq)

  val cnt = RegInit(0.U(BigInt(len).bitLength.W))
  when(newReq) {
    state := s_log2
  }.elsewhen(state === s_log2) {
    // here replace `|` to `+`
    val canSkipShift = (len.U + Log2(bReg)) - Log2(aValx2Reg)
    cnt   := Mux(divBy0, 0.U, Mux(canSkipShift >= (len - 1).U, (len - 1).U, canSkipShift))
    state := s_shift
  }.elsewhen(state === s_shift) {
    shiftReg := aValx2Reg << cnt
    state    := s_compute
  }.elsewhen(state === s_compute) {
    val enough = hi.asUInt >= bReg.asUInt
    shiftReg := Cat(Mux(enough, hi - bReg, hi)(len - 1, 0), lo, enough)
    cnt      := cnt + 1.U
    when(cnt === (len - 1).U) { state := s_finish }
  }.elsewhen(state === s_finish) {
    when(io.out.ready) {
      state := s_idle
    }
  }

  val r    = hi(len, 1)
  val resQ = Mux(qSignReg, -lo, lo)
  val resR = Mux(aSignReg, -r, r)
  io.out.bits := Cat(resR, resQ)

  io.out.valid := (state === s_finish)
  io.in.ready  := (state === s_idle)
}
