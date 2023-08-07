package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait CTermImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait SignalRefImpl { self: SignalRef =>
    override val relatedSignals = tpe.physical match {
      case Reg => RelatedSignals(Set.empty, Set.empty, Set(name.toString() + "$now"))
      case _   => RelatedSignals(Set.empty, Set.empty, Set(name.toString()))
    }
  }
  trait CApplyImpl { self: CApply =>
    override val relatedSignals: RelatedSignals = operands.map(_.relatedSignals).reduce(_ ++ _)
    override def toString: String =
      s"${op.toString}(${operands.map(_.toString).reduce(_ + ", " + _)})"
  }

  trait ConnectImpl { self: Connect =>
    val tpe = left.tpe.updatedPhysical(Node)
    override val relatedSignals: RelatedSignals = (left.tpe.physical match {
      case Reg =>
        RelatedSignals(Set(left.name.toString() + "$next"), Set.empty, Set.empty)
      case _ =>
        RelatedSignals(Set(left.name.toString()), Set.empty, Set.empty)
    }) ++ expr.relatedSignals
  }
  trait BulkConnectImpl { self: BulkConnect =>
    val tpe = EmptyMType
  }

  trait WhenImpl { self: When =>
    val tpe = EmptyMType
    override val relatedSignals: RelatedSignals = {
      val whenRS     = whenBody.map(_.relatedSignals).fold(RelatedSignals.empty)(_ ++ _)
      val otherRS    = otherBody.map(_.relatedSignals).fold(RelatedSignals.empty)(_ ++ _)
      val fully      = whenRS.fully.intersect(otherRS.fully)
      val partially  = whenRS.partially ++ otherRS.partially ++ (whenRS.fully ++ otherRS.fully -- fully)
      val dependency = whenRS.dependency ++ otherRS.dependency ++ cond.relatedSignals.dependency
      RelatedSignals(fully, partially, dependency)
    }
  }
  trait AssertImpl { self: Assert =>
    val tpe                     = EmptyMType
    override val relatedSignals = exp.relatedSignals
  }
  trait SwitchImpl { self: Switch =>
    val tpe = EmptyMType
  }
  trait SubModuleRunImpl { self: SubModuleRun =>
    val tpe = EmptyMType
  }

}
