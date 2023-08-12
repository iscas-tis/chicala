package chicala.ast

import scala.tools.nsc.Global

trait COps { self: ChicalaAst =>
  val global: Global
  import global._

  // operators
  sealed abstract class COp

  sealed abstract class CCalculOp extends COp
  case object VecSelect           extends CCalculOp // vec()
  case object Slice               extends CCalculOp // a()
  case object LogiNot             extends CCalculOp // `!a`
  case object Not                 extends CCalculOp // ~a
  case object Negative            extends CCalculOp // -a

  case object Add      extends CCalculOp // +
  case object Minus    extends CCalculOp // -
  case object Multiply extends CCalculOp // *

  case object And    extends CCalculOp // &
  case object Xor    extends CCalculOp // ^
  case object LShift extends CCalculOp // <<
  case object RShift extends CCalculOp // >>

  case object Equal     extends CCalculOp // ===
  case object NotEqual  extends CCalculOp // =/=
  case object GreaterEq extends CCalculOp // >=

  case object LogiAnd extends CCalculOp // &&
  case object LogiOr  extends CCalculOp // ||

  case object AsUInt extends CCalculOp // .asUInt
  case object AsSInt extends CCalculOp // .asSInt

  sealed abstract class CUtilOp extends COp
  case object Mux               extends CUtilOp
  case object Cat               extends CUtilOp
  case object Fill              extends CUtilOp
  case object Log2              extends CUtilOp

}
