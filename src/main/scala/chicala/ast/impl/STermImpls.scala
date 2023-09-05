package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait STermImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait STermImpl { self: STerm =>
    override def replaced(replaceMap: Map[String, MStatement]): STerm = {
      replaceMap.get(this.toString()).getOrElse(this).asInstanceOf[STerm]
    }
  }

  trait SApplyImpl { self: SApply =>
    override val relatedIdents = {
      val argsDependency = args
        .filter(_.tpe.isSignalType)
        .map(_.relatedIdents.dependency)
        .foldLeft(Set.empty[String])(_ ++ _)
      // FIXME: inner block dependency
      RelatedIdents(Set.empty, Set.empty, argsDependency)
    }
  }

  trait SBlockImpl { self: SBlock =>
    override val relatedIdents =
      if (body.isEmpty) RelatedIdents.empty
      else body.map(_.relatedIdents).reduce(_ ++ _)
  }

  trait SForImpl { self: SFor =>
    val tpe: MType = EmptyMType
  }
  trait SIfImpl { self: SIf =>
    override val relatedIdents = {
      val thenRS     = thenp.relatedIdents
      val elseRS     = elsep.relatedIdents
      val fully      = thenRS.fully.intersect(elseRS.fully)
      val partially  = thenRS.partially ++ elseRS.partially ++ (thenRS.fully ++ elseRS.fully -- fully)
      val dependency = thenRS.dependency ++ elseRS.dependency ++ cond.relatedIdents.dependency
      RelatedIdents(fully, partially, dependency)
    }
  }
  trait STupleImpl { self: STuple =>
    override val relatedIdents = args.map(_.relatedIdents).reduce(_ ++ _)

    override def replaced(r: Map[String, MStatement]): STuple = {
      replacedThis(r) match {
        case STuple(args, tpe) =>
          STuple(args.map(_.replaced(r)), tpe.replaced(r))
        case _ =>
          reportError(NoPosition, "`replaced` should keep data type not changed")
          this
      }
    }

    def size = args.size
  }
  trait SFunctionImpl { self: SFunction =>
    val tpe = StFunc
  }
  trait SAssignImpl { self: SAssign =>
    val tpe = lhs.tpe
  }
}
