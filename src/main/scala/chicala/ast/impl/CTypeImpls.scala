package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.MTypes

trait CTypeImpls { self: MTypes =>
  val global: Global
  import global._

  trait CTypeImpl { self: CType =>
    def updatedPhysical(newPhysical: CPhysical): CType
    def updatedDriction(newDirection: CDirection): CType
    def subSignals: Set[String] = Set.empty
  }
  object CType {
    def empty: CType = Bool(Node, Undirect)
  }

  trait WithPhysicalInfo {}

  trait UIntImpl { self: UInt =>
    def updatedPhysical(newPhysical: CPhysical): UInt = UInt(newPhysical, direction)
    def updatedDriction(newDirection: CDirection): UInt = newDirection match {
      case Input    => UInt(physical, Input)
      case Output   => UInt(physical, Output)
      case Flipped  => UInt(physical, direction.flipped)
      case Undirect => UInt(physical, direction)
    }
  }
  trait SIntImpl { self: SInt =>
    def updatedPhysical(newPhysical: CPhysical): SInt = SInt(newPhysical, direction)
    def updatedDriction(newDirection: CDirection): SInt = newDirection match {
      case Input    => SInt(physical, Input)
      case Output   => SInt(physical, Output)
      case Flipped  => SInt(physical, direction.flipped)
      case Undirect => SInt(physical, direction)
    }
  }
  trait BoolImpl { self: Bool =>
    def updatedPhysical(newPhysical: CPhysical): Bool = Bool(newPhysical, direction)
    def updatedDriction(newDirection: CDirection): Bool = newDirection match {
      case Input    => Bool(physical, Input)
      case Output   => Bool(physical, Output)
      case Flipped  => Bool(physical, direction.flipped)
      case Undirect => Bool(physical, direction)
    }
  }
  trait VecImpl { self: Vec =>
    def updatedPhysical(newPhysical: CPhysical): Vec =
      Vec(tparam.updatedPhysical(newPhysical))
    def updatedDriction(newDirection: CDirection): Vec =
      Vec(tparam.updatedDriction(newDirection))
  }
  trait BundleImpl { self: Bundle =>
    def updatedPhysical(newPhysical: CPhysical): Bundle =
      Bundle(signals.map { case (n, t) => (n, t.updatedPhysical(newPhysical)) })
    def updatedDriction(newDirection: CDirection): Bundle =
      Bundle(signals.map { case (n, t) => (n, t.updatedDriction(newDirection)) })

    override def subSignals: Set[String] = signals
      .map { case (termName, cDataType) =>
        cDataType match {
          case b: Bundle => b.subSignals.map(s => s"${termName.toString()}.${s}")
          case _         => List(termName.toString())
        }
      }
      .flatten
      .toSet
  }
}
