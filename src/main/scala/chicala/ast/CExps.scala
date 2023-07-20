package chicala.ast

import scala.tools.nsc.Global

trait CExps { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CExp {
    def signals: Set[String]
  }
  object CExp {
    def empty = NoneExp
  }

  case class Lit(literal: Literal, info: SignalInfo) extends CExp {
    def signals: Set[String] = Set.empty
  }
  case class SignalRef(name: Tree, info: SignalInfo) extends CExp {
    def signals: Set[String] = Set(name.toString())
  }
  case object NoneExp extends CExp {
    def signals: Set[String] = Set.empty
  }

  // operators
  sealed abstract class COp extends CExp
  sealed abstract class CUnaryOp(val inner: CExp) extends COp {
    def signals: Set[String] = inner.signals
  }
  sealed abstract class CBinaryOp(val left: CExp, val right: CExp) extends COp {
    def signals: Set[String] = left.signals ++ right.signals
  }
  sealed abstract class CTernaryOp(val a: CExp, val b: CExp, val c: CExp) extends COp {
    def signals: Set[String] = a.signals ++ b.signals ++ c.signals
  }

  case class Not(override val inner: CExp)                            extends CUnaryOp(inner)
  case class Add(override val left: CExp, override val right: CExp)   extends CBinaryOp(left, right)
  case class Or(override val left: CExp, override val right: CExp)    extends CBinaryOp(left, right)
  case class And(override val left: CExp, override val right: CExp)   extends CBinaryOp(left, right)
  case class Equal(override val left: CExp, override val right: CExp) extends CBinaryOp(left, right)
  case class Mux(condition: CExp, ifTrue: CExp, ifFalse: CExp)        extends CTernaryOp(condition, ifTrue, ifFalse)
}
