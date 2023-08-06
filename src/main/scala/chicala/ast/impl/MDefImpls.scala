package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait MDefImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait MValDefImpl { self: MValDef =>
    def name: TermName
    def tpe: MType
  }

  trait CValDefImpl { self: CValDef =>
    def name: TermName
    def tpe: CType
  }

  trait IoDefImpl { self: IoDef =>
    override val relatedSignals = RelatedSignals(
      tpe match {
        case b: Bundle => b.subSignals.map(s => s"${name.toString()}.${s}")
        case _         => Set(name.toString())
      },
      Set.empty,
      Set.empty
    )
  }
  trait WireDefImpl { self: WireDef =>
    override val relatedSignals = RelatedSignals(Set(name.toString()), Set.empty, Set.empty)
  }
  trait NodeDefImpl { self: NodeDef =>
    override val relatedSignals =
      RelatedSignals(Set(name.toString()), Set.empty, Set.empty) ++ rhs.relatedSignals
  }

  trait EnumDefImpl { self: EnumDef =>
    override val relatedSignals =
      RelatedSignals(names.map(_.toString()).toSet, Set.empty, Set.empty)
    def inner: List[CValDef] = names
      .zip(0 until names.size)
      .map { case (name, i) =>
        NodeDef(name, info, Lit(SLiteral(i, StInt), info))
      }
  }

}
