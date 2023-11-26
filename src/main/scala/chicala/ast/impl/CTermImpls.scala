package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait CTermImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait LitImpl { self: Lit =>
    val relatedIdents = litExp.relatedIdents ++ RelatedIdents.used(tpe.usedVal)
  }
  trait SignalRefImpl { self: SignalRef =>
    val relatedIdents = {
      val dependency = tpe.allSignals(name.toString(), false)
      RelatedIdents(Set.empty, Set.empty, dependency, Set.empty, Set.empty)
    }
  }
  trait CApplyImpl { self: CApply =>
    val relatedIdents: RelatedIdents = operands.map(_.relatedIdents).reduce(_ ++ _)
    override def toString: String =
      s"${op.toString}(${operands.map(_.toString).reduce(_ + ", " + _)})"

    override def replaced(r: Map[String, MStatement]): CApply = {
      replacedThis(r) match {
        case CApply(op, tpe, operands) =>
          CApply(op, tpe.replaced(r), operands.map(_.replaced(r)))
        case _ =>
          reportError(NoPosition, "`replaced` should keep data type not changed")
          this
      }
    }
  }

  trait ConnectImpl { self: Connect =>
    val tpe = left.tpe.asInstanceOf[SignalType].updatedPhysical(Node)
    val relatedIdents: RelatedIdents = {
      val fully = left match {
        case SignalRef(name, tpe) => tpe.allSignals(name.toString(), true)
        case _                    => left.relatedIdents.dependency
      }
      RelatedIdents(fully, Set.empty, Set.empty, Set.empty, Set.empty)
    } ++ expr.relatedIdents

    override def replaced(replaceMap: Map[String, MStatement]): Connect = {
      val tmp  = replaceMap.get(this.toString()).getOrElse(this).asInstanceOf[Connect]
      val tmpb = Connect(tmp.left.replaced(replaceMap), tmp.expr.replaced(replaceMap))
      tmpb
    }

    def expands: List[Connect] = {
      (left, expr) match {
        case (SignalRef(ln, lt: Bundle), SignalRef(rn, rt: Bundle)) =>
          val subNames = lt.signals.keySet.intersect(rt.signals.keySet).toList
          subNames
            .map(n =>
              Connect(
                SignalRef(Select(ln, n), lt.signals(n)),
                SignalRef(Select(rn, n), rt.signals(n))
              ).expands
            )
            .flatten
        case _ => List(this)
      }
    }
  }

  trait WhenImpl { self: When =>
    val tpe = EmptyMType
    val relatedIdents: RelatedIdents = {
      val whenRI  = whenp.relatedIdents
      val otherRI = otherp.relatedIdents
      val condRI  = cond.relatedIdents

      val sum = whenRI ++ otherRI ++ condRI

      val fully      = whenRI.fully.intersect(otherRI.fully)
      val partially  = whenRI.partially ++ otherRI.partially ++ (whenRI.fully ++ otherRI.fully -- fully)
      val dependency = sum.dependency
      val updated    = sum.updated
      val used       = sum.used

      RelatedIdents(fully, partially, dependency, updated, used)
    }
  }
  trait AssertImpl { self: Assert =>
    val tpe           = EmptyMType
    val relatedIdents = exp.relatedIdents
  }
  trait SwitchImpl { self: Switch =>
    val tpe = EmptyMType
    def appended(v: MTerm, branchp: MStatement): Switch = {
      this.copy(branchs = branchs.appended((v, branchp)))
    }
    val relatedIdents = cond.relatedIdents ++
      branchs.map(x => x._1.relatedIdents ++ x._2.relatedIdents).foldLeft(RelatedIdents.empty)(_ ++ _)
  }
  trait SubModuleRunImpl { self: SubModuleRun =>
    val tpe = EmptyMType

    val relatedIdents: RelatedIdents = {
      val nameStr   = name.toString()
      val fully     = outputNames.map(_.toString()).toSet
      val partially = Set.empty[String]
      val dependency = Set(nameStr) ++
        inputRefs.map(_.relatedIdents.dependency).reduce(_ ++ _)

      RelatedIdents(fully, partially, dependency, Set.empty, Set.empty)
    }
  }

}
