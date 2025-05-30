package example.rocketchip

import chisel3._
import chisel3.util._

class MultiplierReq(dataBits: Int, tagBits: Int) extends Bundle {
  val fn  = UInt(4.W) // aluFn.SZ_ALU_FN
  val dw  = UInt(1.W)
  val in1 = UInt(dataBits.W)
  val in2 = UInt(dataBits.W)
  val tag = UInt(tagBits.W)
}

class MultiplierResp(dataBits: Int, tagBits: Int) extends Bundle {
  val data = UInt(dataBits.W)
  val tag  = UInt(tagBits.W)
}

class MultiplierIO(val dataBits: Int, val tagBits: Int) extends Bundle {
  val req = Flipped(new Bundle {
    val ready = Input(Bool())
    val valid = Output(Bool())
    val bits  = Output(new MultiplierReq(dataBits, tagBits))
  })
  val resp = new Bundle {
    val ready = Input(Bool())
    val valid = Output(Bool())
    val bits  = Output(new MultiplierResp(dataBits, tagBits))
  }
}

class Mul(
    mulUnroll: Int = 1,
    divUnroll: Int = 1,
    mulEarlyOut: Boolean = false,
    divEarlyOut: Boolean = false,
    divEarlyOutGranularity: Int = 1,
    w: Int,
    nXpr: Int = 32
) extends Module {
  // require(mulUnroll > 0)
  private def minMulLatency = if (mulEarlyOut) 2 else w / mulUnroll
  def minLatency: Int       = minMulLatency

  val io = IO(new MultiplierIO(w, log2Up(nXpr)))

  val mulw     = if (mulUnroll == 0) w else (w + mulUnroll - 1) / mulUnroll * mulUnroll
  val fastMulW = if (mulUnroll == 0) false else w / 2 > mulUnroll && w % (2 * mulUnroll) == 0 // 5

  val s_ready :: s_neg_inputs :: s_mul :: s_div :: s_dummy :: s_neg_output :: s_done_mul :: s_done_div :: Nil = Enum(8)

  val state = RegInit(s_ready)

  val req = Reg(new MultiplierReq(w, log2Up(nXpr)))
  val count = Reg(
    UInt(log2Ceil((if (mulUnroll == 0) w else (w + mulUnroll - 1) / mulUnroll * mulUnroll) / mulUnroll).W)
  )
  val neg_out = Reg(Bool())          // 10
  val isHi    = Reg(Bool())
  val resHi   = Reg(Bool())
  val divisor = Reg(UInt((w + 1).W)) // div only needs w bits
  val remainder = Reg(
    UInt((2 * (if (mulUnroll == 0) w else (w + mulUnroll - 1) / mulUnroll * mulUnroll) + 2).W)
  ) // div only needs 2*w+1 bits

  val cmdMul, cmdHi, lhsSigned, rhsSigned = WireInit(false.B) // 15 - 18
  if (mulUnroll != 0) { // 19
    switch(io.req.bits.fn) {
      is(0.U) { // aluFn.FN_MUL
        cmdMul    := true.B  // 20.1.1.1.1
        cmdHi     := false.B // 20.1.1.1.2
        lhsSigned := false.B // 20.1.1.1.3
        rhsSigned := false.B // 20.1.1.1.4
      }
    }
  }

  // require(w == 32 || w == 64)
  def halfWidth(reqDw: UInt) = (w > 32).B && reqDw === false.B // 20

  def sext(x: UInt, halfW: Bool, signed: Bool) = { // 21
    val sign = signed && Mux(halfW, x(w / 2 - 1), x(w - 1))
    val hi   = Mux(halfW, Fill(w / 2, sign), x(w - 1, w / 2))
    (Cat(hi, x(w / 2 - 1, 0)), sign)
  }
  val (lhs_in, lhs_sign) = sext(io.req.bits.in1, halfWidth(io.req.bits.dw), lhsSigned)
  val (rhs_in, rhs_sign) = sext(io.req.bits.in2, halfWidth(io.req.bits.dw), rhsSigned)

  val subtractor        = remainder(2 * w, w) - divisor
  val result            = Mux(resHi, remainder(2 * w, w + 1), remainder(w - 1, 0)) // 25
  val negated_remainder = -result

  val mulReg         = Cat(remainder(2 * mulw + 1, w + 1), remainder(w - 1, 0))
  val mplierSign     = remainder(w)
  val mplier         = mulReg(mulw - 1, 0)
  val accum          = mulReg(2 * mulw, mulw).asSInt // 30
  val mpcand         = divisor.asSInt
  val prod           = Cat(mplierSign, mplier(mulUnroll - 1, 0)).asSInt * mpcand + accum
  val nextMulReg     = Cat(prod, mplier(mulw - 1, mulUnroll))
  val nextMplierSign = count === (mulw / mulUnroll - 2).U && neg_out

  val eOutMask = (
    (BigInt(-1) << mulw).S >>
      (count * mulUnroll.U)(log2Up(mulw) - 1, 0)
  )(mulw - 1, 0) // 35
  val eOut = (mulEarlyOut).B &&
    count =/= (mulw / mulUnroll - 1).U &&
    count =/= 0.U &&
    !isHi &&
    (mplier & ~eOutMask) === 0.U
  val eOutRes = mulReg >>
    (mulw.U - count * mulUnroll.U)(log2Up(mulw) - 1, 0)
  val nextMulReg1 = Cat(
    nextMulReg(2 * mulw, mulw),
    Mux(eOut, eOutRes, nextMulReg)(mulw - 1, 0)
  )

  if (mulUnroll != 0) when(state === s_mul) { // 39
    remainder := Cat(nextMulReg1 >> w, nextMplierSign, nextMulReg1(w - 1, 0))

    count := count + 1.U
    when(eOut || count === (mulw / mulUnroll - 1).U) {
      state := s_done_mul
      resHi := isHi
    }
  }
  when(io.resp.ready && io.resp.valid) { // 40
    state := s_ready
  }
  when(io.req.ready && io.req.valid) { // 41
    state     := Mux(cmdMul, s_mul, s_ready)
    isHi      := cmdHi
    resHi     := false.B
    count     := (if (fastMulW) Mux[UInt](cmdMul && halfWidth(io.req.bits.dw), (w / mulUnroll / 2).U, 0.U) else 0.U)
    neg_out   := Mux(cmdHi, lhs_sign, lhs_sign =/= rhs_sign)
    divisor   := Cat(rhs_sign, rhs_in)
    remainder := lhs_in
    req       := io.req.bits
  }

  val outMul = (state & (s_done_mul ^ s_done_div)) === (s_done_mul & ~s_done_div)
  val loOut  = Mux(fastMulW.B && halfWidth(req.dw) && outMul, result(w - 1, w / 2), result(w / 2 - 1, 0))
  val hiOut  = Mux(halfWidth(req.dw), Fill(w / 2, loOut(w / 2 - 1)), result(w - 1, w / 2))
  io.resp.bits.tag := req.tag // 45

  io.resp.bits.data := Cat(hiOut, loOut)
  io.resp.valid     := (state === s_done_mul)
  io.req.ready      := state === s_ready // 48
}
