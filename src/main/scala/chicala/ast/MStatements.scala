package chicala.ast

import scala.tools.nsc.Global
import chicala.ChicalaPlugin

trait MStatements { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class MStatement {
    def relatedSignals: RelatedSignals = RelatedSignals.empty
  }

  // MTerm
  sealed abstract class MTerm extends MStatement {
    def tpe: MType

    def isEmpty = this == EmptyMTerm
  }
  object MTerm {
    def empty = EmptyMTerm
  }

  sealed abstract class CTerm extends MTerm

  case class Lit(litExp: STerm, tpe: CType) extends CTerm
  case class SignalRef(name: Tree, info: CType) extends CTerm {
    val tpe = EmptyMType

    override val relatedSignals = RelatedSignals(Set.empty, Set.empty, Set(name.toString))

  }
  case class CApply(op: COp, tpe: CType, operands: List[MTerm]) extends CTerm {
    override val relatedSignals: RelatedSignals = operands.map(_.relatedSignals).reduce(_ ++ _)

    override def toString: String =
      s"${op.toString}(${operands.map(_.toString).reduce(_ + ", " + _)})"
  }

  case class Connect(left: SignalRef, expr: MTerm) extends CTerm {
    val tpe = EmptyMType
    override val relatedSignals: RelatedSignals =
      RelatedSignals(Set(left.name.toString()), Set.empty, Set.empty) ++
        expr.relatedSignals
  }
  case class BulkConnect() extends CTerm {
    val tpe = EmptyMType
  }

  case class When(
      val cond: MTerm,
      val whenBody: List[MStatement],
      val otherBody: List[MStatement],
      val hasElseWhen: Boolean = false
  ) extends CTerm {
    val tpe = EmptyMType
    override val relatedSignals: RelatedSignals = {
      val whenRS     = whenBody.map(_.relatedSignals).fold(RelatedSignals.empty)(_ ++ _)
      val otherRS    = otherBody.map(_.relatedSignals).fold(RelatedSignals.empty)(_ ++ _)
      val fully      = whenRS.fully.intersect(otherRS.fully)
      val partially  = whenRS.partially ++ otherRS.partially ++ (whenRS.fully ++ otherRS.fully -- fully)
      val dependency = whenRS.dependency ++ otherRS.dependency ++ cond.relatedSignals.dependency
      RelatedSignals(fully, partially, dependency)
    }
  }

  case class Assert(exp: MTerm) extends CTerm {
    val tpe                     = EmptyMType
    override val relatedSignals = exp.relatedSignals
  }
  case class Switch() extends CTerm {
    val tpe = EmptyMType
  }
  case class SubModuleRun() extends CTerm {
    val tpe = EmptyMType
  }

  sealed abstract class STerm                                    extends MTerm
  case class SApply(fun: SSelect, args: List[MTerm], tpe: MType) extends STerm
  case class SSelect(select: Select, tpe: MType)                 extends STerm
  case class SBlock(body: List[MStatement], tpe: MType)          extends STerm
  case class SLiteral(literal: Literal, tpe: MType)              extends STerm
  case class SFor() extends STerm {
    val tpe: MType = EmptyMType
  }
  case class SIf(tpe: MType)    extends STerm
  case class SMatch(tpe: MType) extends STerm
  object SBlock {
    def empty = SBlock(List.empty, EmptyMType)
  }

  case object EmptyMTerm extends MTerm {
    val tpe = EmptyMType
  }

  // MDef

  sealed abstract class MDef extends MStatement {}

  sealed abstract class MValDef extends MDef {
    def name: TermName
    def tpe: MType
  }

  sealed abstract class CValDef extends MValDef {
    def name: TermName
    def tpe: CType
  }
  case class IoDef(name: TermName, tpe: CType) extends CValDef {
    override val relatedSignals: RelatedSignals = RelatedSignals(
      tpe match {
        case b: Bundle => b.subSignals.map(s => s"${name.toString()}.${s}")
        case _         => Set(name.toString())
      },
      Set.empty,
      Set.empty
    )
  }
  case class WireDef(name: TermName, tpe: CType) extends CValDef {
    override val relatedSignals: RelatedSignals = RelatedSignals(Set(name.toString()), Set.empty, Set.empty)
  }
  case class RegDef(
      name: TermName,
      tpe: CType,
      someInit: Option[MTerm] = None,
      someNext: Option[MTerm] = None,
      someEnable: Option[MTerm] = None
  ) extends CValDef

  case class NodeDef(name: TermName, tpe: CType, rhs: MTerm) extends CValDef {
    override val relatedSignals: RelatedSignals =
      RelatedSignals(Set(name.toString()), Set.empty, Set.empty) ++ rhs.relatedSignals
  }
  case class SValDef(name: TermName, tpe: SType, rhs: MTerm, isVar: Boolean = false) extends MValDef

  sealed abstract class MUnapplyDef extends MDef
  case class EnumDef(names: List[TermName], info: CType) extends MUnapplyDef {
    override val relatedSignals: RelatedSignals =
      RelatedSignals(names.map(_.toString()).toSet, Set.empty, Set.empty)
    def inner: List[CValDef] = names
      .zip(0 until names.size)
      .map { case (name, i) =>
        NodeDef(name, info, Lit(SLiteral(Literal(Constant(i)), EmptyMType), info))
      }
  }
  case class SUnapplyDef(
      names: List[TermName],
      rhs: MTerm,
      tupleTyp: List[Type]
  ) extends MUnapplyDef

  case class SDefDef(name: TermName, vparamss: List[List[SValDef]], tpt: Tree, body: SBlock) extends MDef

  case class SubModuleDef() extends MDef

}
