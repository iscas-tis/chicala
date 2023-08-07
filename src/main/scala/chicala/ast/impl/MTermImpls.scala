package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait MTermImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait MStatementImpl { self: MStatement =>
    def relatedSignals: RelatedSignals = RelatedSignals.empty
  }

  trait MTermImpl { self: MTerm =>
    def tpe: MType

    def isEmpty = this == EmptyMTerm
  }
  object MTerm {
    def empty = EmptyMTerm
  }
}
