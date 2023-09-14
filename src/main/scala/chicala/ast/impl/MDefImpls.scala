package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.ast.MStatements

trait MDefImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait MValDefImpl { self: MValDef =>
    def name: TermName
    def tpe: MType
  }

  trait SubModuleDefImpl { self: SubModuleDef =>
    val relatedIdents = RelatedIdents(
      Set(name.toString()),
      Set.empty,
      Set.empty,
      Set(name.toString()),
      Set.empty
    )
  }

  trait SignalDefImpl { self: SignalDef =>
    def name: TermName
    def tpe: SignalType
  }

  trait IoDefImpl { self: IoDef =>
    val relatedIdents = {
      val newVals =
        tpe match {
          case b: Bundle => b.subSignals.map(s => s"${name.toString()}.${s}")
          case _         => Set(name.toString())
        }
      RelatedIdents(
        newVals,
        Set.empty,
        Set.empty,
        newVals,
        tpe.usedVal
      )
    }
  }
  trait WireDefImpl { self: WireDef =>
    val relatedIdents = {
      val newVals = tpe.allSignals(name.toString(), false)
      RelatedIdents(newVals, Set.empty, Set.empty, newVals, Set.empty)
    } ++ someInit.map(_.relatedIdents).getOrElse(RelatedIdents.empty)

    override def replaced(r: Map[String, MStatement]): WireDef = {
      replacedThis(r) match {
        case WireDef(name, tpe, someInit, isVar) =>
          WireDef(name, tpe.replaced(r), someInit.map(_.replaced(r)), isVar)
        case _ =>
          reportError(NoPosition, "`replaced` should keep data type not changed")
          this
      }
    }
  }
  trait RegDefImpl { self: RegDef =>
    val relatedIdents = {
      val fully    = tpe.allSignals(name.toString(), false) ++ tpe.allSignals(name.toString(), true)
      val nextRI   = someNext.map(_.relatedIdents).getOrElse(RelatedIdents.empty)
      val enableRI = someEnable.map(_.relatedIdents).getOrElse(RelatedIdents.empty)
      RelatedIdents(fully, Set.empty, Set.empty, fully, Set.empty) ++ nextRI ++ enableRI
    }

  }
  trait NodeDefImpl { self: NodeDef =>
    val relatedIdents =
      RelatedIdents(Set(name.toString()), Set.empty, Set.empty, Set(name.toString()), Set.empty) ++
        rhs.relatedIdents
  }

  trait SValDefImpl { self: SValDef =>
    val relatedIdents = RelatedIdents.updated(Set(name.toString()))

  }

  trait EnumDefImpl { self: EnumDef =>
    val relatedIdents = {
      val newVals = names.map(_.toString()).toSet
      RelatedIdents(newVals, Set.empty, Set.empty, newVals, Set.empty)
    }
    def inner: List[SignalDef] = names
      .zip(0 until names.size)
      .map { case (name, i) =>
        NodeDef(name, tpe, Lit(SLiteral(i, StInt), tpe))
      }
  }

  trait SUnapplyDefImpl { self: SUnapplyDef =>
    val relatedIdents = {
      val nameAndType = names.map(_.toString()).zip(tpe.tparams)
      val signals     = nameAndType.collect({ case (name, tpe) if !tpe.isSType => name }).toSet
      val vals        = nameAndType.collect({ case (name, tpe) if tpe.isSType => name }).toSet

      RelatedIdents(signals, Set.empty, Set.empty, vals ++ signals, Set.empty) ++
        rhs.relatedIdents
    }
  }

  trait SDefDefImpl { self: SDefDef =>
    val relatedIdents = RelatedIdents.empty
  }

}
