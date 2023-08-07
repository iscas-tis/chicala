package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait STermImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait SApplyImpl { self: SApply =>
    override val relatedSignals = RelatedSignals(
      Set.empty,
      Set.empty,
      args
        .map(x =>
          x.tpe match {
            case _: CType => Some(x)
            case _        => None
          }
        )
        .flatten
        .map(_.relatedSignals)
        .foldLeft(RelatedSignals.empty)(_ ++ _)
        .dependency
    )
  }

  trait SBlockImpl { self: SBlock =>
    override val relatedSignals =
      if (body.isEmpty) RelatedSignals.empty
      else body.map(_.relatedSignals).reduce(_ ++ _)
  }

  trait SForImpl { self: SFor =>
    val tpe: MType = EmptyMType
  }
  trait SIfImpl { self: SIf =>
    override val relatedSignals = {
      val thenRS     = thenp.relatedSignals
      val elseRS     = elsep.relatedSignals
      val fully      = thenRS.fully.intersect(elseRS.fully)
      val partially  = thenRS.partially ++ elseRS.partially ++ (thenRS.fully ++ elseRS.fully -- fully)
      val dependency = thenRS.dependency ++ elseRS.dependency ++ cond.relatedSignals.dependency
      RelatedSignals(fully, partially, dependency)
    }
  }
  trait STupleImpl { self: STuple =>
    override val relatedSignals = args.map(_.relatedSignals).reduce(_ ++ _)
  }

}
