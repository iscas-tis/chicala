package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait MTermImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait MStatementImpl { self: MStatement =>
    def tpe: MType
    def relatedSignals: RelatedSignals = RelatedSignals.empty
    def replacedThis(replaceMap: Map[String, MStatement]): MStatement = {
      replaceMap.get(this.toString()).getOrElse(this)
    }
    def replaced(replaceMap: Map[String, MStatement]): MStatement = {
      // FIXME: need recursively replacing all terms
      replacedThis(replaceMap)
    }
  }

  trait MTermImpl { self: MTerm =>
    def tpe: MType

    override def replaced(replaceMap: Map[String, MStatement]): MTerm = {
      replaceMap.get(this.toString()).getOrElse(this).asInstanceOf[MTerm]
    }

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
