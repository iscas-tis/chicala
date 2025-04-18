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

  case class Lit(litExp: STerm, tpe: GroundType)                     extends CTerm with LitImpl
  case class SignalRef(name: Tree, tpe: SignalType)                  extends CTerm with SignalRefImpl
  case class CApply(op: COp, tpe: SignalType, operands: List[MTerm]) extends CTerm with CApplyImpl

  case class Connect(left: MTerm, expr: MTerm) extends CTerm with ConnectImpl

  case class When(
      val cond: MTerm,
      val whenp: MStatement,
      val otherp: MStatement,
      val hasElseWhen: Boolean = false
  ) extends CTerm
      with WhenImpl
  case class Assert(exp: MTerm) extends CTerm with AssertImpl
  case class Switch(
      cond: MTerm,
      branchs: List[(MTerm, MStatement)]
  ) extends CTerm
      with SwitchImpl
  case class SubModuleRun(
      name: Tree,
      inputRefs: List[SignalRef],
      outputNames: List[TermName],
      moduleType: SubModule,
      inputSignals: List[(String, SignalType)],
      outputSignals: List[(String, SignalType)]
  ) extends CTerm
      with SubModuleRunImpl

  // STerm
  sealed abstract class STerm                                                        extends MTerm with STermImpl
  case class SApply(fun: STerm, args: List[MTerm], tpe: MType)                       extends STerm with SApplyImpl
  case class SSelect(from: MTerm, name: TermName, tpe: MType)                        extends STerm with SSelectImpl
  case class SBlock(body: List[MStatement], tpe: MType)                              extends STerm with SBlockImpl
  case class SLiteral(value: Any, tpe: MType)                                        extends STerm with SLiteralImpl
  case class SIdent(name: TermName, tpe: MType)                                      extends STerm with SIdentImpl
  case class SIf(cond: STerm, thenp: MTerm, elsep: MTerm, tpe: MType)                extends STerm with SIfImpl
  case class SMatch(selector: MTerm, cases: List[SCaseDef], tpe: MType)              extends STerm with SMatchImpl
  case class SCaseDef(tupleNames: List[(TermName, MType)], casep: MTerm, tpe: MType) extends STerm with SCaseDefImpl
  case class STuple(args: List[MTerm], tpe: StTuple)                                 extends STerm with STupleImpl
  case class SLib(name: String, tpe: SType)                                          extends STerm with SLibImpl
  case class SFunction(vparams: List[MValDef], funcp: MTerm)                         extends STerm with SFunctionImpl
  case class SAssign(lhs: MTerm, rhs: MTerm)                                         extends STerm with SAssignImpl

  case object EmptyMTerm extends MTerm with EmptyMTermImpl

  // MDef
  sealed abstract class MDef extends MStatement

  // MValDef
  sealed abstract class MValDef extends MDef with MValDefImpl

  sealed abstract class CValDef extends MValDef

  case class SubModuleDef(name: TermName, tpe: SubModule, args: List[MTerm]) extends CValDef with SubModuleDefImpl

  sealed abstract class SignalDef extends CValDef with SignalDefImpl

  case class IoDef(name: TermName, tpe: SignalType) extends SignalDef with IoDefImpl
  case class WireDef(
      name: TermName,
      tpe: SignalType,
      someInit: Option[MTerm] = None,
      isVar: Boolean = false
  ) extends SignalDef
      with WireDefImpl
  case class RegDef(
      name: TermName,
      tpe: SignalType,
      someInit: Option[MTerm] = None,
      someNext: Option[MTerm] = None,
      someEnable: Option[MTerm] = None
  ) extends SignalDef
      with RegDefImpl
  case class NodeDef(name: TermName, tpe: SignalType, rhs: MTerm, isVar: Boolean = false)
      extends SignalDef
      with NodeDefImpl

  case class SValDef(name: TermName, tpe: SType, rhs: MTerm, isVar: Boolean = false) extends MValDef with SValDefImpl

  // other Def
  sealed abstract class MUnapplyDef extends MDef

  case class EnumDef(names: List[TermName], tpe: UInt)                    extends MUnapplyDef with EnumDefImpl
  case class SUnapplyDef(names: List[TermName], rhs: MTerm, tpe: StTuple) extends MUnapplyDef with SUnapplyDefImpl

  case class SDefDef(name: TermName, vparamss: List[List[MValDef]], tpe: MType, defp: MStatement)
      extends MDef
      with SDefDefImpl
}
