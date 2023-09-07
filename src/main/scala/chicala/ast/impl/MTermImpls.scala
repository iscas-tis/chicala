package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait MTermImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait MStatementImpl { self: MStatement =>
    def tpe: MType
    def relatedIdents: RelatedIdents = RelatedIdents.empty
    def replacedThis(replaceMap: Map[String, MStatement]): MStatement = {
      replaceMap.getOrElse(this.toString(), this)
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

  /** related idents, include val and def
    *
    * @param fully
    *   fully updated val
    * @param partially
    *   partially updated val, e.g. in different branch
    * @param dependency
    *   dependent indents
    */
  case class RelatedIdents(val fully: Set[String], val partially: Set[String], val dependency: Set[String]) {
    def ++(that: RelatedIdents): RelatedIdents = {
      RelatedIdents(
        this.fully ++ that.fully,
        this.partially ++ that.partially,
        this.dependency ++ that.dependency
      )
    }
    def removedAll(set: IterableOnce[String]): RelatedIdents = {
      RelatedIdents(
        fully.removedAll(set),
        partially.removedAll(set),
        dependency.removedAll(set)
      )
    }
  }
  object RelatedIdents {
    def empty = RelatedIdents(Set.empty, Set.empty, Set.empty)
  }
}
