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

  case class RelatedSignals(val fully: Set[String], val partially: Set[String], val dependency: Set[String]) {
    def ++(that: RelatedSignals): RelatedSignals = {
      RelatedSignals(
        this.fully ++ that.fully,
        this.partially ++ that.partially,
        this.dependency ++ that.dependency
      )
    }
    def removedAll(set: IterableOnce[String]): RelatedSignals = {
      RelatedSignals(
        fully.removedAll(set),
        partially.removedAll(set),
        dependency.removedAll(set)
      )
    }
  }
  object RelatedSignals {
    def empty = RelatedSignals(Set.empty, Set.empty, Set.empty)
  }
}
