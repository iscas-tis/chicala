package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait MTermImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait MStatementImpl { self: MStatement =>
    def tpe: MType
    def relatedIdents: RelatedIdents
    def replacedThis(replaceMap: Map[String, MStatement]): MStatement = {
      replaceMap.getOrElse(this.toString(), this)
    }
    def replaced(replaceMap: Map[String, MStatement]): MStatement = {
      // FIXME: need recursively replacing all terms
      replacedThis(replaceMap)
    }
    def isEmpty  = this == EmptyMTerm
    def nonEmpty = !isEmpty
  }

  trait MTermImpl { self: MTerm =>
    def tpe: MType

    override def replaced(replaceMap: Map[String, MStatement]): MTerm = {
      replaceMap.get(this.toString()).getOrElse(this).asInstanceOf[MTerm]
    }
  }
  object MTerm {
    def empty = EmptyMTerm
  }

  trait EmptyMTermImpl {
    val tpe           = EmptyMType
    val relatedIdents = RelatedIdents.empty
  }

  /** related idents, include val and def
    *
    * @param fully
    *   fully connected signal
    * @param partially
    *   partially connected signal, e.g. in different branch
    * @param dependency
    *   dependent signal
    * @param updated
    *   val, var and func define or updated
    * @param used
    *   val, var and func used
    */
  case class RelatedIdents(
      val fully: Set[String],
      val partially: Set[String],
      val dependency: Set[String],
      val updated: Set[String],
      val used: Set[String]
  ) {
    def ++(that: RelatedIdents): RelatedIdents = {
      RelatedIdents(
        this.fully ++ that.fully,
        this.partially ++ that.partially,
        this.dependency ++ that.dependency,
        this.updated ++ that.updated,
        this.used ++ that.used
      )
    }
    def removedAll(set: IterableOnce[String]): RelatedIdents = {
      RelatedIdents(
        fully.removedAll(set),
        partially.removedAll(set),
        dependency.removedAll(set),
        updated.removedAll(set),
        used.removedAll(set)
      )
    }
    def usedAll = used ++ fully ++ partially ++ dependency
  }
  object RelatedIdents {
    def empty = RelatedIdents(Set.empty, Set.empty, Set.empty, Set.empty, Set.empty)

    def fully(fSet: Set[String])   = RelatedIdents(fSet, Set.empty, Set.empty, Set.empty, Set.empty)
    def updated(uSet: Set[String]) = RelatedIdents(Set.empty, Set.empty, Set.empty, uSet, Set.empty)
    def used(uSet: Set[String])    = RelatedIdents(Set.empty, Set.empty, Set.empty, Set.empty, uSet)
  }

}
