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
    override val relatedIdents = RelatedIdents(
      Set(name.toString()),
      Set.empty,
      Set.empty
    )
  }

  trait SignalDefImpl { self: SignalDef =>
    def name: TermName
    def tpe: SignalType
  }

  trait IoDefImpl { self: IoDef =>
    override val relatedIdents = RelatedIdents(
      tpe match {
        case b: Bundle => b.subSignals.map(s => s"${name.toString()}.${s}")
        case _         => Set(name.toString())
      },
      Set.empty,
      Set.empty
    )
  }
  trait WireDefImpl { self: WireDef =>
    override val relatedIdents =
      RelatedIdents(tpe.allSignals(name.toString(), false), Set.empty, Set.empty) ++
        someInit.map(_.relatedIdents).getOrElse(RelatedIdents.empty)

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
    override val relatedIdents = {
      val fully = tpe.allSignals(name.toString(), false) ++ tpe.allSignals(name.toString(), true)
      val nextRS = someNext match {
        case None        => RelatedIdents.empty
        case Some(value) => value.relatedIdents
      }
      val enableRS = someEnable match {
        case None        => RelatedIdents.empty
        case Some(value) => value.relatedIdents
      }
      RelatedIdents(fully, Set.empty, Set.empty) ++ nextRS ++ enableRS
    }

  }
  trait NodeDefImpl { self: NodeDef =>
    override val relatedIdents =
      RelatedIdents(Set(name.toString()), Set.empty, Set.empty) ++ rhs.relatedIdents
  }

  trait EnumDefImpl { self: EnumDef =>
    override val relatedIdents =
      RelatedIdents(names.map(_.toString()).toSet, Set.empty, Set.empty)
    def inner: List[SignalDef] = names
      .zip(0 until names.size)
      .map { case (name, i) =>
        NodeDef(name, tpe, Lit(SLiteral(i, StInt), tpe))
      }
  }

  trait SUnapplyDefImpl { self: SUnapplyDef =>
    override val relatedIdents =
      RelatedIdents(names.map(_.toString()).toSet, Set.empty, Set.empty) ++
        rhs.relatedIdents
  }

  trait SDefDefImpl { self: SDefDef =>
    override val relatedIdents = RelatedIdents.empty
  }

}
