package chicala.ast

import scala.tools.nsc.Global

trait CSignalInfos { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CPhysicalType
  case object Io     extends CPhysicalType
  case object Wire   extends CPhysicalType
  case object Reg    extends CPhysicalType
  case object Node   extends CPhysicalType
  case object Symbol extends CPhysicalType

  sealed abstract class CDirection {
    def flipped: CDirection
  }
  case object Input extends CDirection {
    def flipped: CDirection = Output
  }
  case object Output extends CDirection {
    def flipped: CDirection = Input
  }
  case object Flipped extends CDirection {
    def flipped: CDirection = Undirect
  }
  case object Undirect extends CDirection {
    def flipped: CDirection = Undirect
  }

  sealed abstract class CDataType {
    def updateDriction(newDirection: CDirection): CDataType

    def subSignals: Set[String] = Set.empty
  }
  case class UInt(width: Tree, direction: CDirection) extends CDataType {
    def updateDriction(newDirection: CDirection): CDataType = newDirection match {
      case Input    => UInt(width, Input)
      case Output   => UInt(width, Output)
      case Flipped  => UInt(width, direction.flipped)
      case Undirect => UInt(width, direction)
    }
  }
  case class SInt(width: Tree, direction: CDirection) extends CDataType {
    def updateDriction(newDirection: CDirection): CDataType = newDirection match {
      case Input    => SInt(width, Input)
      case Output   => SInt(width, Output)
      case Flipped  => SInt(width, direction.flipped)
      case Undirect => SInt(width, direction)
    }
  }
  case class Bool(direction: CDirection) extends CDataType {
    def updateDriction(newDirection: CDirection): CDataType = newDirection match {
      case Input    => Bool(Input)
      case Output   => Bool(Output)
      case Flipped  => Bool(direction.flipped)
      case Undirect => Bool(direction)
    }
  }
  case class Bundle(signals: Map[TermName, CDataType]) extends CDataType {
    def updateDriction(newDirection: CDirection): CDataType = {
      Bundle(
        signals.map { case (n, t) => (n, t.updateDriction(newDirection)) }
      )
    }
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

  case class Vec(size: Tree, cDataType: CDataType) extends CDataType {
    def updateDriction(newDirection: CDirection): CDataType = {
      Vec(size, cDataType.updateDriction(newDirection))
    }
  }

  case class SignalInfo(physicalType: CPhysicalType, dataType: CDataType)

  object SignalInfo {
    def empty = SignalInfo(Node, Bool(Undirect))
  }

}
