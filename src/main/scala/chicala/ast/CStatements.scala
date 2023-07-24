package chicala.ast

import scala.tools.nsc.Global

trait CStatements extends SStatements { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CStatement {
    val relatedSignals: RelatedSignals
  }

  sealed abstract class SignalDef extends CStatement

  case class IoDef(name: TermName, info: SignalInfo, circuitName: TypeName) extends SignalDef {
    val relatedSignals: RelatedSignals = RelatedSignals(
      info.dataType match {
        case b: Bundle => b.subSignals.map(s => s"${name.toString()}.${s}")
        case _         => Set(name.toString())
      },
      Set.empty,
      Set.empty
    )
  }

  case class WireDef(name: TermName, info: SignalInfo, circuitName: TypeName) extends SignalDef {
    // not empty when WireDef with init val
    val relatedSignals: RelatedSignals = RelatedSignals(Set(name.toString()), Set.empty, Set.empty)
  }

  case class RegDef() extends SignalDef {
    // not empty when RegDef with init val
    val relatedSignals: RelatedSignals = RelatedSignals.empty
  }
  case class NodeDef() extends SignalDef {
    // for now
    val relatedSignals: RelatedSignals = RelatedSignals.empty
  }

  case class Connect(left: SignalRef, expr: CExp) extends CStatement {
    val relatedSignals: RelatedSignals =
      RelatedSignals(Set(left.name.toString()), Set.empty, expr.signals)
  }
  case class BulkConnect() extends CStatement {
    // for now
    val relatedSignals: RelatedSignals = RelatedSignals.empty
  }

  case class When(cond: CExp, whenBody: List[CStatement], otherBody: List[CStatement]) extends CStatement {
    val relatedSignals: RelatedSignals = {
      val whenRS     = whenBody.map(_.relatedSignals).fold(RelatedSignals.empty)(_ ++ _)
      val otherRS    = otherBody.map(_.relatedSignals).fold(RelatedSignals.empty)(_ ++ _)
      val fully      = whenRS.fully.intersect(otherRS.fully)
      val partially  = whenRS.partially ++ otherRS.partially ++ (whenRS.fully ++ otherRS.fully -- fully)
      val dependency = whenRS.dependency ++ otherRS.dependency ++ cond.signals
      RelatedSignals(fully, partially, dependency)
    }
  }

  case class Assert(exp: CExp) extends CStatement {
    val relatedSignals: RelatedSignals = RelatedSignals(Set.empty, Set.empty, exp.signals)
  }

  // Scala Extention
  // Use `trait xxxImpl` to implement in other file,
  // but keep class `sealed` in this file

  sealed abstract class SStatement(val tree: Tree) extends CStatement with SStatementImpl {
    val relatedSignals: RelatedSignals = RelatedSignals.empty
  }
  sealed abstract class SDef(valOrDefDef: ValOrDefDef) extends SStatement(valOrDefDef)

  case class SDefDef(defDef: DefDef) extends SDef(defDef) with SDefDefImpl
  case class SValDef(valDef: ValDef) extends SDef(valDef) with SValDefImpl
  case class SApply(appl: Apply)     extends SStatement(appl) with SApplyImpl
  case class SSelect(select: Select) extends SStatement(select) with SSelectImpl
  case class SBlock(block: Block)    extends SStatement(block) with SBlockImpl
}
