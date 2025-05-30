package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait CTypeImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait CTypeImpl { self: CType =>
    def replaced(replaceMap: Map[String, MStatement]): CType = this
  }

  trait SignalTypeImpl { self: SignalType =>
    def physical: CPhysical

    def updatedPhysical(newPhysical: CPhysical): SignalType
    def updatedDriction(newDirection: CDirection): SignalType
    def setInferredWidth: SignalType = this
    def subSignals: Set[String]      = Set.empty
    def allSignals(parentName: String, leftSide: Boolean): Set[String] = physical match {
      case Reg => Set(if (leftSide) Reg.nowSignal(parentName) else Reg.nextSignal(parentName))
      case _   => Set(parentName)
    }

    override def replaced(replaceMap: Map[String, MStatement]): SignalType = {
      this
    }

    def isInput: Boolean
    def isOutput: Boolean
    def isReg: Boolean = physical match {
      case Reg => true
      case _   => false
    }

    /** Use "_" flatten all signals in Bundle structure
      */
    def flatten(name: String): List[(String, SignalType)] = List(name -> this)

    def usedVal: Set[String]
  }
  trait GroundTypeImpl { self: GroundType =>
    def direction: CDirection

    def isInput: Boolean = direction match {
      case Input => true
      case _     => false
    }
    def isOutput: Boolean = direction match {
      case Output => true
      case _      => false
    }
  }

  object SignalType {
    def empty: SignalType = Bool(Node, Undirect)
  }

  trait UIntImpl { self: UInt =>
    def updatedWidth(newWidth: CSize): UInt           = copy(width = newWidth)
    def updatedPhysical(newPhysical: CPhysical): UInt = copy(physical = newPhysical)
    def updatedDriction(newDirection: CDirection): UInt = newDirection match {
      case Input    => copy(direction = Input)
      case Output   => copy(direction = Output)
      case Flipped  => copy(direction = direction.flipped)
      case Undirect => copy(direction = Undirect)
    }
    override def setInferredWidth = copy(width = InferredSize)
    override def replaced(r: Map[String, MStatement]): UInt =
      this.copy(width = width.replaced(r))
    def usedVal: Set[String] = width match {
      case KnownSize(width) => width.relatedIdents.used
      case _                => Set.empty
    }
  }
  trait SIntImpl { self: SInt =>
    def updatedWidth(newWidth: CSize): SInt           = copy(width = newWidth)
    def updatedPhysical(newPhysical: CPhysical): SInt = copy(physical = newPhysical)
    def updatedDriction(newDirection: CDirection): SInt = newDirection match {
      case Input    => copy(direction = Input)
      case Output   => copy(direction = Output)
      case Flipped  => copy(direction = direction.flipped)
      case Undirect => copy(direction = Undirect)
    }
    override def setInferredWidth = copy(width = InferredSize)
    override def replaced(r: Map[String, MStatement]): SInt =
      this.copy(width = width.replaced(r))
    def usedVal: Set[String] = width match {
      case KnownSize(width) => width.relatedIdents.used
      case _                => Set.empty
    }
  }
  trait BoolImpl { self: Bool =>
    def updatedPhysical(newPhysical: CPhysical): Bool = copy(physical = newPhysical)
    def updatedDriction(newDirection: CDirection): Bool = newDirection match {
      case Input    => copy(direction = Input)
      case Output   => copy(direction = Output)
      case Flipped  => copy(direction = direction.flipped)
      case Undirect => copy(direction = Undirect)
    }
    override def replaced(r: Map[String, MStatement]): Bool = this

    def usedVal: Set[String] = Set.empty
  }

  trait VecImpl { self: Vec =>
    def updatedPhysical(newPhysical: CPhysical): Vec =
      copy(physical = newPhysical, tparam = tparam.updatedPhysical(newPhysical))
    def updatedDriction(newDirection: CDirection): Vec =
      copy(tparam = tparam.updatedDriction(newDirection))

    override def replaced(r: Map[String, MStatement]): Vec =
      this.copy(size = size.replaced(r), tparam = tparam.replaced(r))

    def isInput  = tparam.isInput
    def isOutput = tparam.isOutput

    def usedVal: Set[String] = (size match {
      case KnownSize(width) => width.relatedIdents.used
      case _                => Set.empty
    }) ++ tparam.usedVal
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

    override def replaced(r: Map[String, MStatement]): Bundle =
      this.copy(
        signals = signals.map({ case (name, tpe) => name -> tpe.replaced(r) })
      )

    // Bundle it-self cannot be a Input or Output
    def isInput  = false
    def isOutput = false

    override def flatten(name: String): List[(String, SignalType)] = signals
      .map { case (termName, cDataType) =>
        cDataType.flatten(s"${name}_${termName}")
      }
      .reduce(_ ++ _)

    def usedVal: Set[String] = signals.map(_._2.usedVal).reduce(_ ++ _)
  }

  trait RegImpl {
    private val nowSuffix  = "$now"
    private val nextSuffix = "$next"

    def nowSignal(signal: String)  = signal + "." + nowSuffix
    def nextSignal(signal: String) = signal + "." + nextSuffix

    def nowSignals(signals: Set[String])  = signals.map(nowSignal(_))
    def nextSignals(signals: Set[String]) = signals.map(nextSignal(_))
  }

  trait CSizeImpl { self: CSize =>
    def replaced(r: Map[String, MStatement]): CSize = {
      this match {
        case KnownSize(width) =>
          KnownSize(width.replaced(r))
        case _ => this
      }
    }
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
