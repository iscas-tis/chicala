package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait CTermImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait SignalRefImpl { self: SignalRef =>
    override val relatedIdents = {
      val dependency = tpe.allSignals(name.toString(), false)
      RelatedIdents(Set.empty, Set.empty, dependency)
    }
  }
  trait CApplyImpl { self: CApply =>
    override val relatedIdents: RelatedIdents = operands.map(_.relatedIdents).reduce(_ ++ _)
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
    override val relatedIdents: RelatedIdents = {
      val fully = left match {
        case SignalRef(name, tpe) => tpe.allSignals(name.toString(), true)
        case _                    => left.relatedIdents.dependency
      }
      RelatedIdents(fully, Set.empty, Set.empty)
    } ++ expr.relatedIdents

    override def replaced(replaceMap: Map[String, MStatement]): Connect = {
      val tmp  = replaceMap.get(this.toString()).getOrElse(this).asInstanceOf[Connect]
      val tmpb = Connect(tmp.left.replaced(replaceMap), tmp.expr.replaced(replaceMap))
      tmpb
    }
  }
  trait BulkConnectImpl { self: BulkConnect =>
    val tpe = EmptyMType
  }

  trait WhenImpl { self: When =>
    val tpe = EmptyMType
    override val relatedIdents: RelatedIdents = {
      val whenRS     = whenBody.map(_.relatedIdents).fold(RelatedIdents.empty)(_ ++ _)
      val otherRS    = otherBody.map(_.relatedIdents).fold(RelatedIdents.empty)(_ ++ _)
      val fully      = whenRS.fully.intersect(otherRS.fully)
      val partially  = whenRS.partially ++ otherRS.partially ++ (whenRS.fully ++ otherRS.fully -- fully)
      val dependency = whenRS.dependency ++ otherRS.dependency ++ cond.relatedIdents.dependency
      RelatedIdents(fully, partially, dependency)
    }
  }
  trait AssertImpl { self: Assert =>
    val tpe                    = EmptyMType
    override val relatedIdents = exp.relatedIdents
  }
  trait SwitchImpl { self: Switch =>
    val tpe = EmptyMType
    def appended(v: MTerm, body: List[MStatement]): Switch = {
      this.copy(branchs = branchs.appended((v, body)))
    }
  }
  trait SubModuleRunImpl { self: SubModuleRun =>
    val tpe = EmptyMType

    override val relatedIdents: RelatedIdents = {
      val nameStr   = name.toString()
      val fully     = outputSignals.map({ case (n, _) => s"${outputName}.${n}" }).toSet
      val partially = Set.empty[String]
      val dependency = Set(nameStr) ++
        inputSignals.map({ case (n, _) => s"${nameStr}_${n}" })

      RelatedIdents(fully, partially, dependency)
    }
  }

}
