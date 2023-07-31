package chicala.ast

import scala.tools.nsc.Global

trait CExps { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CExp(val info: SignalInfo) {
    def signals: Set[String]
    def isEmpty = this match {
      case EmptyExp => true
      case _        => false
    }
  }
  object CExp {
    def empty = EmptyExp
  }

  case class Lit(val litExp: SExp, override val info: SignalInfo) extends CExp(info) {
    def signals: Set[String] = Set.empty
  }
  case class SignalRef(val name: Tree, override val info: SignalInfo) extends CExp(info) {
    def signals: Set[String] = Set(name.toString)
  }
  case object EmptyExp extends CExp(SignalInfo.empty) {
    def signals: Set[String] = Set.empty
  }

  // operators
  case class CApply(val op: COp, override val info: SignalInfo, val operands: List[CExp]) extends CExp(info) {
    def signals: Set[String] = operands.map(_.signals).reduce(_ ++ _)

    override def toString: String =
      s"${op.toString}(${operands.map(_.toString).reduce(_ + ", " + _)})"
  }

  sealed abstract class COp

  sealed abstract class CCalculOp extends COp
  case object Slice               extends CCalculOp // a()
  case object Not                 extends CCalculOp // !a
  case object UnaryMinus          extends CCalculOp // -a

  case object Add       extends CCalculOp // +
  case object Minus     extends CCalculOp // -
  case object Or        extends CCalculOp // ||
  case object And       extends CCalculOp // &&
  case object Xor       extends CCalculOp // ^
  case object LShift    extends CCalculOp // <<
  case object Equal     extends CCalculOp // ===
  case object GreaterEq extends CCalculOp // >=

  case object AsUInt extends CCalculOp // .asUInt

  sealed abstract class CUtilOp extends COp
  case object Mux               extends CUtilOp
  case object Cat               extends CUtilOp
  case object Fill              extends CUtilOp

  // scala extension

  case class SExp(val sStatement: SStatement, override val info: SignalInfo) extends CExp(info) {
    def signals: Set[String] = sStatement.relatedSignals.dependency
  }
}
