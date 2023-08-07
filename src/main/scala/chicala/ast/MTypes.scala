package chicala.ast

import scala.tools.nsc.Global

import chicala.ast.impl.CTypeImpls

trait MTypes extends CTypeImpls { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class MType

  // CType
  sealed abstract class CType extends MType with CTypeImpl

  case class UInt(width: CSize, physical: CPhysical, direction: CDirection) extends CType with UIntImpl
  case class SInt(width: CSize, physical: CPhysical, direction: CDirection) extends CType with SIntImpl
  case class Bool(physical: CPhysical, direction: CDirection)               extends CType with BoolImpl

  case class Vec(size: CSize, physical: CPhysical, tparam: CType)       extends CType with VecImpl
  case class Bundle(physical: CPhysical, signals: Map[TermName, CType]) extends CType with BundleImpl

  // CPhysical
  sealed abstract class CPhysical
  case object Io   extends CPhysical
  case object Wire extends CPhysical
  case object Reg  extends CPhysical
  case object Node extends CPhysical

  // CDirection
  sealed abstract class CDirection { def flipped: CDirection }
  case object Input    extends CDirection { def flipped: CDirection = Output   }
  case object Output   extends CDirection { def flipped: CDirection = Input    }
  case object Flipped  extends CDirection { def flipped: CDirection = Undirect }
  case object Undirect extends CDirection { def flipped: CDirection = Undirect }

  // CSize
  sealed abstract class CSize
  case class KnownSize(width: STerm) extends CSize
  case object InferredSize           extends CSize
  case object UnknownSize            extends CSize

  // SType
  sealed abstract class SType              extends MType
  case object StInt                        extends SType
  case object StString                     extends SType
  case object StBigInt                     extends SType
  case class StTuple(tparams: List[MType]) extends SType
  case object StFunc                       extends SType

  case object EmptyMType extends MType

  // Companion Object
  object UInt extends UIntObjImpl
  object SInt extends SIntObjImpl
  object Bool extends BoolObjImpl
}
