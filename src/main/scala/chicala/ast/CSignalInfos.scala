package chicala.ast

import scala.tools.nsc.Global

trait CSignalInfos { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CPhysicalType
  case object Io   extends CPhysicalType
  case object Wire extends CPhysicalType
  case object Reg  extends CPhysicalType
  case object Node extends CPhysicalType

  sealed abstract class CDirection {
    def flipped: CDirection
  }
  object CDirection {
    def fromTree(tree: Tree): CDirection = {
      tree match {
        /*
          TypeApply(
            Select(Select(Ident(chisel3), chisel3.Input), TermName("apply")),
            List(TypeTree())
          )
         */
        case TypeApply(Select(Select(Ident(TermName(chisel3)), tpe), TermName("apply")), _) =>
          tpe match {
            case TypeName("Input")   => Input
            case TypeName("Output")  => Output
            case TypeName("Flipped") => Flipped
            case _                   => Undirect
          }
        case _ => Undirect
      }
    }
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
  object CDataType {
    def fromTree(tree: Tree): CDataType = {
      tree match {
        /* Apply(Apply(<Input(_)>, List(<UInt(width.W)>)), List(<someCompileOptions>)) */
        case Apply(Apply(typeApply, args), _) => {
          val direction = CDirection.fromTree(typeApply)
          val cDataType = CDataType.fromTree(args.head)
          cDataType.updateDriction(direction)
        }
        /* Apply(<UInt(_)>, List(<width.W>)) */
        case Apply(Select(Select(cp, tpe), TermName("apply")), args) if Chisel3Package(cp) => {
          assert(args.length <= 1, "width args at mose one")
          val someWidth = args match {
            case Select(Apply(Select(cp, TermName("fromIntToWidth")), List(w)), TermName("W")) :: next
                if Chisel3Package(cp) =>
              Some(w)
            case _ => None
          }
          tpe match {
            case TermName("UInt") => UInt(someWidth.get, Undirect)
            case TermName("SInt") => UInt(someWidth.get, Undirect)
            case TermName("Bool") => Bool(Undirect)
            case _                => Bool(Undirect)
          }
        }
      }
    }
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
  // case class Vec() extends CDataType

  case class SignalInfo(physicalType: CPhysicalType, dataType: CDataType)

  object SignalInfo {
    def empty = SignalInfo(Node, Bool(Undirect))
  }

}
