package chicala.ast

import scala.tools.nsc.Global

import chicala.ast.impl._

trait MStatements extends MTermImpls with CTermImpls with STermImpls with MDefImpls { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class MStatement extends MStatementImpl

  // MTerm
  sealed abstract class MTerm extends MStatement with MTermImpl

  // CTerm
  sealed abstract class CTerm extends MTerm

  case class Lit(litExp: STerm, tpe: CType)                     extends CTerm
  case class SignalRef(name: Tree, tpe: CType)                  extends CTerm with SignalRefImpl
  case class CApply(op: COp, tpe: CType, operands: List[MTerm]) extends CTerm with CApplyImpl

  case class Connect(left: SignalRef, expr: MTerm) extends CTerm with ConnectImpl
  case class BulkConnect()                         extends CTerm with BulkConnectImpl

  case class When(
      val cond: MTerm,
      val whenBody: List[MStatement],
      val otherBody: List[MStatement],
      val hasElseWhen: Boolean = false
  ) extends CTerm
      with WhenImpl
  case class Assert(exp: MTerm) extends CTerm with AssertImpl
  case class Switch()           extends CTerm with SwitchImpl
  case class SubModuleRun()     extends CTerm with SubModuleRunImpl

  // STerm
  sealed abstract class STerm                                  extends MTerm
  case class SApply(fun: STerm, args: List[MTerm], tpe: MType) extends STerm
  case class SSelect(from: STerm, name: TermName, tpe: MType)  extends STerm
  case class SBlock(body: List[MStatement], tpe: MType)        extends STerm
  case class SLiteral(value: Any, tpe: MType)                  extends STerm
  case class SIdent(name: TermName, tpe: MType)                extends STerm
  case class SFor()                                            extends STerm with SForImpl
  case class SIf(tpe: MType)                                   extends STerm
  case class SMatch(tpe: MType)                                extends STerm
  case class STuple(args: List[MTerm], tpe: StTuple)           extends STerm
  case class SLib(name: String, tpe: SType)                    extends STerm

  case object EmptyMTerm extends MTerm { val tpe = EmptyMType }

  // MDef
  sealed abstract class MDef extends MStatement

  sealed abstract class MValDef extends MDef with MValDefImpl

  sealed abstract class CValDef extends MValDef with CValDefImpl

  case class IoDef(name: TermName, tpe: CType)   extends CValDef with IoDefImpl
  case class WireDef(name: TermName, tpe: CType) extends CValDef with WireDefImpl
  case class RegDef(
      name: TermName,
      tpe: CType,
      someInit: Option[MTerm] = None,
      someNext: Option[MTerm] = None,
      someEnable: Option[MTerm] = None
  ) extends CValDef
  case class NodeDef(name: TermName, tpe: CType, rhs: MTerm) extends CValDef with NodeDefImpl

  case class SValDef(name: TermName, tpe: SType, rhs: MTerm, isVar: Boolean = false) extends MValDef

  sealed abstract class MUnapplyDef extends MDef

  case class EnumDef(names: List[TermName], tpe: CType)                   extends MUnapplyDef with EnumDefImpl
  case class SUnapplyDef(names: List[TermName], rhs: MTerm, tpe: StTuple) extends MUnapplyDef

  case class SDefDef(name: TermName, vparamss: List[List[MValDef]], tpe: MType, body: SBlock) extends MDef
  case class SubModuleDef()                                                                   extends MDef

}
