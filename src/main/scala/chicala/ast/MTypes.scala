package chicala.ast

import scala.tools.nsc.Global

import chicala.ast.impl._

trait MTypes extends MTypeImpls with CTypeImpls with STypeImpls { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class MType extends MTypeImpl

  // CType
  sealed abstract class CType extends MType with CTypeImpl

  case class SubModule(fullName: String, ioDef: IoDef) extends CType

  // SignalType
  sealed abstract class SignalType extends CType with SignalTypeImpl
  sealed abstract class GroundType extends SignalType with GroundTypeImpl

  case class UInt(width: CSize, physical: CPhysical, direction: CDirection) extends GroundType with UIntImpl
  case class SInt(width: CSize, physical: CPhysical, direction: CDirection) extends GroundType with SIntImpl
  case class Bool(physical: CPhysical, direction: CDirection)               extends GroundType with BoolImpl

  case class Vec(size: CSize, physical: CPhysical, tparam: SignalType)       extends SignalType with VecImpl
  case class Bundle(physical: CPhysical, signals: Map[TermName, SignalType]) extends SignalType with BundleImpl

  // CPhysical
  sealed abstract class CPhysical
  case object Io   extends CPhysical
  case object Wire extends CPhysical
  case object Reg  extends CPhysical with RegImpl
  case object Node extends CPhysical

  // CDirection
  sealed abstract class CDirection { def flipped: CDirection }
  case object Input    extends CDirection { def flipped: CDirection = Output   }
  case object Output   extends CDirection { def flipped: CDirection = Input    }
  case object Flipped  extends CDirection { def flipped: CDirection = Undirect }
  case object Undirect extends CDirection { def flipped: CDirection = Undirect }

  // CSize
  sealed abstract class CSize        extends CSizeImpl
  case class KnownSize(width: STerm) extends CSize
  case object InferredSize           extends CSize
  case object UnknownSize            extends CSize

  // SType
  sealed abstract class SType extends MType with STypeImpl

  case object StInt                        extends SType
  case object StString                     extends SType
  case object StBigInt                     extends SType
  case object StBoolean                    extends SType
  case class StTuple(tparams: List[MType]) extends SType with StTupleImpl
  case class StSeq(tparam: MType)          extends SType with StSeqImpl
  case object StFunc                       extends SType
  case object StUnit                       extends SType
  case object StAny                        extends SType
  case class StWrapped(str: String)        extends SType with StWrappedImpl

  case object EmptyMType extends MType with EmptyMTypeImpl

  // Companion Object
  object UInt extends UIntObjImpl
  object SInt extends SIntObjImpl
  object Bool extends BoolObjImpl

  object KnownSize extends KnownSizeObjImpl

}
