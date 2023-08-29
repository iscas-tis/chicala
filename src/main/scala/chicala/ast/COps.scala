package chicala.ast

import scala.tools.nsc.Global

trait COps { self: ChicalaAst =>
  val global: Global
  import global._

  // operators
  sealed abstract class COp

  sealed abstract class CFrontOp extends COp
  case object LogiNot            extends CFrontOp // `!a`
  case object Not                extends CFrontOp // ~a
  case object Negative           extends CFrontOp // -a

  sealed abstract class CBinaryOp extends COp

  case object Add      extends CBinaryOp // +
  case object Minus    extends CBinaryOp // -
  case object Multiply extends CBinaryOp // `*`

  case object And    extends CBinaryOp // &
  case object Or     extends CBinaryOp // |
  case object Xor    extends CBinaryOp // ^
  case object LShift extends CBinaryOp // <<
  case object RShift extends CBinaryOp // >>

  case object Equal     extends CBinaryOp // ===
  case object NotEqual  extends CBinaryOp // =/=
  case object GreaterEq extends CBinaryOp // >=

  case object LogiAnd extends CBinaryOp // &&
  case object LogiOr  extends CBinaryOp // ||

  sealed abstract class CBackOp extends COp

  case object Slice extends CBackOp // a()

  case object VecSelect extends CBackOp // vec()
  case object VecTake   extends CBackOp // vec.take()
  case object VecLast   extends CBackOp // vec.last

  case object AsUInt extends CBackOp // .asUInt
  case object AsSInt extends CBackOp // .asSInt
  case object AsBool extends CBackOp // .asBool

  sealed abstract class CUtilOp extends COp
  case object Mux               extends CUtilOp
  case object MuxLookup         extends CUtilOp
  case object Cat               extends CUtilOp
  case object Fill              extends CUtilOp
  case object Log2              extends CUtilOp

}
