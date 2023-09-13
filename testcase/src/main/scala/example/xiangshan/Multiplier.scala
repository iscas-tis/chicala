package example.xiangshan

import chisel3._
import chisel3.util._

class C22 extends Module {
  val io = IO(new Bundle() {
    val in  = Input(Vec(2, UInt(1.W)))
    val out = Output(Vec(2, UInt(1.W)))
  })
  val temp = Wire(Vec(1, UInt(2.W)))
  for (i <- 0 until temp.length) {
    val (a, b) = (io.in(0)(i), io.in(1)(i))
    val sum    = a ^ b
    val cout   = a & b
    temp(i) := Cat(cout, sum)
  }
  for (i <- 0 until io.out.length) {
    io.out(i) := Cat(temp.reverse map (_(i)))
  }
}
class C32 extends Module {
  val io = IO(new Bundle() {
    val in  = Input(Vec(3, UInt(1.W)))
    val out = Output(Vec(2, UInt(1.W)))
  })
  val temp = Wire(Vec(1, UInt(2.W)))
  for (i <- 0 until temp.length) {
    val (a, b, cin) = (io.in(0)(i), io.in(1)(i), io.in(2)(i))
    val a_xor_b     = a ^ b
    val a_and_b     = a & b
    val sum         = a_xor_b ^ cin
    val cout        = a_and_b | (a_xor_b & cin)
    temp(i) := Cat(cout, sum)
  }
  for (i <- 0 until io.out.length) {
    io.out(i) := Cat(temp.reverse map (_(i)))
  }
}
class C53 extends Module {
  val io = IO(new Bundle() {
    val in  = Input(Vec(5, UInt(1.W)))
    val out = Output(Vec(3, UInt(1.W)))
  })
  val FAs0, FAs1 = Module(new C32)
  FAs0.io.in := io.in.take(3)
  val tmp1 = VecInit(FAs0.io.out(0), io.in(3), io.in(4))
  FAs1.io.in := tmp1
  val tmp2 = VecInit(FAs1.io.out(0), FAs0.io.out(1), FAs1.io.out(1))
  io.out := tmp2
}

class ArrayMulDataModule(len: Int) extends Module {
  val io = IO(new Bundle() {
    val a, b       = Input(UInt(len.W))
    val regEnables = Input(Vec(2, Bool()))
    val result     = Output(UInt((2 * len).W))
  })
  def signExt(a: UInt, len: Int): UInt = {
    val aLen    = a.getWidth
    val signBit = a(aLen - 1)
    if (aLen >= len) a(len - 1, 0) else Cat(Fill(len - aLen, signBit), a)
  }

  val (a, b) = (io.a, io.b)

  val b_sext, bx2, neg_b, neg_bx2 = Wire(UInt((len + 1).W))
  b_sext  := signExt(b, len + 1)
  bx2     := b_sext << 1
  neg_b   := (~b_sext).asUInt()
  neg_bx2 := neg_b << 1

  val columns: Array[Seq[Bool]] = Array.fill(2 * len)(Seq())

  var last_x = WireInit(0.U(3.W))
  for (i <- Range(0, len, 2)) {
    val x =
      if (i == 0) Cat(a(1, 0), 0.U(1.W))
      else if (i + 1 == len) signExt(a(i, i - 1), 3)
      else a(i + 1, i - 1)
    val pp_temp = MuxLookup(
      x,
      0.U,
      Seq(
        1.U -> b_sext,
        2.U -> b_sext,
        3.U -> bx2,
        4.U -> neg_bx2,
        5.U -> neg_b,
        6.U -> neg_b
      )
    )
    val s = pp_temp(len)
    val t = MuxLookup(
      last_x,
      0.U(2.W),
      Seq(
        4.U -> 2.U(2.W),
        5.U -> 1.U(2.W),
        6.U -> 1.U(2.W)
      )
    )
    last_x = x
    val (pp, weight) =
      if (i == 0)
        (Cat(~s, s, s, pp_temp), 0)
      else if (i == len - 1 || i == len - 1)
        (Cat(~s, pp_temp, t), i - 2)
      else
        (Cat(1.U(1.W), ~s, pp_temp, t), i - 2)

    for (j <- columns.indices) {
      if (j >= weight && j < (weight + pp.getWidth)) {
        columns(j) = columns(j) :+ pp(j - weight)
      }
    }
  }

  def addOneColumn(col: Seq[Bool], cin: Seq[Bool]): (Seq[Bool], Seq[Bool], Seq[Bool]) = {
    var sum   = Seq[Bool]()
    var cout1 = Seq[Bool]()
    var cout2 = Seq[Bool]()

    val cin_1 = if (cin.nonEmpty) Seq(cin.head) else Nil
    val cin_2 = if (cin.nonEmpty) cin.drop(1) else Nil

    var s_1, c_1_1, c_1_2 = Seq[Bool]()
    var s_2, c_2_1, c_2_2 = Seq[Bool]()
    var tmp1, tmp2        = (Seq[Bool](), Seq[Bool](), Seq[Bool]())

    if (col.size == 1) {
      // do nothing
      sum = col ++ cin
    } else if (col.size == 2) {
      val c22 = Module(new C22)
      c22.io.in := col
      sum = c22.io.out(0).asBool() +: cin
      cout2 = Seq(c22.io.out(1).asBool())
    } else if (col.size == 3) {
      val c32 = Module(new C32)
      c32.io.in := col
      sum = c32.io.out(0).asBool() +: cin
      cout2 = Seq(c32.io.out(1).asBool())
    } else if (col.size == 4) {
      val c53 = Module(new C53)
      c53.io.in := col.take(4) :+ (if (cin.nonEmpty) cin.head else 0.U)
      sum = Seq(c53.io.out(0).asBool()) ++ (if (cin.nonEmpty) cin.drop(1) else Nil)
      cout1 = Seq(c53.io.out(1).asBool())
      cout2 = Seq(c53.io.out(2).asBool())
    } else {
      tmp1 = addOneColumn(col take 4, cin_1)
      tmp2 = addOneColumn(col drop 4, cin_2)

      s_1 = tmp1._1
      c_1_1 = tmp1._2
      c_1_2 = tmp1._3

      s_2 = tmp2._1
      c_2_1 = tmp2._2
      c_2_2 = tmp2._3

      sum = s_1 ++ s_2
      cout1 = c_1_1 ++ c_2_1
      cout2 = c_1_2 ++ c_2_2
    }

    (sum, cout1, cout2)
  }

  def addAll(cols: Array[Seq[Bool]], depth: Int): (UInt, UInt) = {
    var k = 0

    val columns_next = Array.fill(2 * len)(Seq[Bool]())
    var cout1, cout2 = Seq[Bool]()

    if (cols.forall(_.size <= 2)) {
      for (i <- 0 until cols.size) {
        if (cols(i).size == 1) k = i + 1
      }
      (
        Cat(cols.map(_(0)).toIndexedSeq.reverse),
        Cat(Cat(cols.drop(k).map(_(1)).toIndexedSeq.reverse), 0.U(k.W))
      )
    } else {
      for (i <- cols.indices) {
        val (s, c1, c2) = addOneColumn(cols(i), cout1)
        columns_next(i) = s ++ cout2
        cout1 = c1
        cout2 = c2
      }

      addAll(columns_next, depth + 1)
    }
  }

  val (sum, carry) = addAll(cols = columns, depth = 0)

  io.result := sum + carry
}
