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
    val relatedIdents = {
      args.map(_.relatedIdents).foldLeft(RelatedIdents.empty)(_ ++ _) ++ fun.relatedIdents
    }

    override def replaced(r: Map[String, MStatement]): SApply = {
      replacedThis(r) match {
        case SApply(fun, args, tpe) => SApply(fun.replaced(r), args.map(_.replaced(r)), tpe.replaced(r))
        case _ =>
          reportError(NoPosition, "`replaced` should keep data type not changed")
          this
      }
    }
  }
  trait SSelectImpl { self: SSelect =>
    // FIXME
    val relatedIdents = RelatedIdents.empty
  }

  trait SBlockImpl { self: SBlock =>
    val relatedIdents = {
      val newIdent = body
        .filter({
          case _: MDef => true
          case _       => false
        })
        .map(x => x.relatedIdents.updated ++ x.relatedIdents.fully)
        .foldLeft(Set.empty[String])(_ ++ _)

      body
        .map(_.relatedIdents)
        .foldLeft(RelatedIdents.empty)(_ ++ _)
        .removedAll(newIdent)
    }
  }

  trait SLiteralImpl { self: SLiteral =>
    val relatedIdents = RelatedIdents.empty
  }
  trait SIdentImpl { self: SIdent =>
    val relatedIdents = RelatedIdents.used(Set(name.toString()))
  }

  trait SIfImpl { self: SIf =>
    val relatedIdents = {
      val thenRI = thenp.relatedIdents
      val elseRI = elsep.relatedIdents
      val condRI = cond.relatedIdents
      val sum    = thenRI ++ elseRI ++ condRI

      val fully     = thenRI.fully.intersect(elseRI.fully)
      val partially = thenRI.partially ++ elseRI.partially ++ (thenRI.fully ++ elseRI.fully -- fully)

      sum.copy(fully = fully, partially = partially)
    }
  }

  trait SMatchImpl { self: SMatch =>
    // FIXME
    val relatedIdents = RelatedIdents.empty
  }
  trait SCaseDefImpl { self: SCaseDef =>
    // FIXME
    val relatedIdents = RelatedIdents.empty
  }

  trait STupleImpl { self: STuple =>
    val relatedIdents = args.map(_.relatedIdents).reduce(_ ++ _)

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
  trait SLibImpl { self: SLib =>
    val relatedIdents = RelatedIdents.empty
  }
  trait SFunctionImpl { self: SFunction =>
    val tpe           = StFunc
    val relatedIdents = funcp.relatedIdents
  }
  trait SAssignImpl { self: SAssign =>
    val tpe = lhs.tpe
    val relatedIdents =
      RelatedIdents(Set.empty, Set.empty, Set.empty, lhs.relatedIdents.usedAll, rhs.relatedIdents.usedAll)
  }
}
