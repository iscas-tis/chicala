package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.MTypes

trait CTypeImpls { self: MTypes =>
  val global: Global
  import global._

  trait CTypeImpl { self: CType =>
    def physical: CPhysical

    def updatedPhysical(newPhysical: CPhysical): CType
    def updatedDriction(newDirection: CDirection): CType
    def setInferredWidth: CType = this
    def subSignals: Set[String] = Set.empty
    def allSignals(parentName: String, leftSide: Boolean): Set[String] = physical match {
      case Reg => Set(if (leftSide) Reg.nowSignal(parentName) else Reg.nextSignal(parentName))
      case _   => Set(parentName)
    }
  }
  object CType {
    def empty: CType = Bool(Node, Undirect)
  }

  trait UIntImpl { self: UInt =>
    def updatedWidth(newWidth: CSize): UInt           = copy(width = newWidth)
    def updatedPhysical(newPhysical: CPhysical): UInt = copy(physical = newPhysical)
    def updatedDriction(newDirection: CDirection): UInt = newDirection match {
      case Input    => copy(direction = Input)
      case Output   => copy(direction = Output)
      case Flipped  => copy(direction = direction.flipped)
      case Undirect => copy(direction = direction)
    }
    override def setInferredWidth = copy(width = InferredSize)
  }
  trait SIntImpl { self: SInt =>
    def updatedWidth(newWidth: CSize): SInt           = copy(width = newWidth)
    def updatedPhysical(newPhysical: CPhysical): SInt = copy(physical = newPhysical)
    def updatedDriction(newDirection: CDirection): SInt = newDirection match {
      case Input    => copy(direction = Input)
      case Output   => copy(direction = Output)
      case Flipped  => copy(direction = direction.flipped)
      case Undirect => copy(direction = direction)
    }
    override def setInferredWidth = copy(width = InferredSize)
  }
  trait BoolImpl { self: Bool =>
    def updatedPhysical(newPhysical: CPhysical): Bool = copy(physical = newPhysical)
    def updatedDriction(newDirection: CDirection): Bool = newDirection match {
      case Input    => copy(direction = Input)
      case Output   => copy(direction = Output)
      case Flipped  => copy(direction = direction.flipped)
      case Undirect => copy(direction = direction)
    }
  }

  trait VecImpl { self: Vec =>
    def updatedPhysical(newPhysical: CPhysical): Vec =
      copy(physical = newPhysical, tparam = tparam.updatedPhysical(newPhysical))
    def updatedDriction(newDirection: CDirection): Vec =
      copy(tparam = tparam.updatedDriction(newDirection))
  }
  trait BundleImpl { self: Bundle =>
    def updatedPhysical(newPhysical: CPhysical): Bundle = copy(
      physical = newPhysical,
      signals = signals.map { case (n, t) => (n, t.updatedPhysical(newPhysical)) }
    )
    def updatedDriction(newDirection: CDirection): Bundle = copy(
      signals = signals.map { case (n, t) => (n, t.updatedDriction(newDirection)) }
    )

    override def subSignals: Set[String] = signals
      .map { case (termName, cDataType) =>
        cDataType match {
          case b: Bundle => b.subSignals.map(s => s"${termName.toString()}.${s}")
          case _         => List(termName.toString())
        }
      }
      .flatten
      .toSet

    override def allSignals(parentName: String, leftSide: Boolean): Set[String] = {
      val signals = subSignals.map(parentName + "." + _)
      physical match {
        case Reg => if (leftSide) Reg.nextSignals(signals) else Reg.nowSignals(signals)
        case _   => signals
      }
    }
  }

  trait RegImpl {
    private val nowSuffix  = "$now"
    private val nextSuffix = "$next"

    def nowSignal(signal: String)  = signal + "." + nowSuffix
    def nextSignal(signal: String) = signal + "." + nextSuffix

    def nowSignals(signals: Set[String])  = signals.map(nowSignal(_))
    def nextSignals(signals: Set[String]) = signals.map(nextSignal(_))
  }

  trait UIntObjImpl {
    def empty = UInt(UnknownSize, Node, Undirect)
  }
  trait SIntObjImpl {
    def empty = SInt(UnknownSize, Node, Undirect)
  }
  trait BoolObjImpl {
    def empty = Bool(Node, Undirect)
  }
}
