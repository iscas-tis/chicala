package chicala.ast

import scala.tools.nsc.Global

trait CircuitInfos { self: ChicalaAst =>
  val global: Global
  import global._

  case class CircuitInfo(
      val name: TypeName,
      val params: List[SValDef],
      val vals: Map[TermName, MType],
      val funcs: Map[TermName, MType],
      /* use to record EnumDef  */
      val numTmp: Int,
      val enumTmp: Option[(TermName, EnumDef)],
      val tupleTmp: Option[(TermName, SUnapplyDef)]
  ) {
    def updatedParam(sValDef: SValDef): CircuitInfo =
      this.copy(params = params :+ sValDef)

    def updatedVal(termName: TermName, tpe: MType): CircuitInfo =
      this.copy(vals = vals + (termName -> tpe))

    def updatedFunc(termName: TermName, tpe: MType): CircuitInfo =
      this.copy(funcs = funcs + (termName -> tpe))

    def updatedEnumTmp(num: Int, et: Option[(TermName, EnumDef)]): CircuitInfo =
      this.copy(numTmp = num, enumTmp = et)
    def updatedTupleTmp(num: Int, tt: Option[(TermName, SUnapplyDef)]): CircuitInfo =
      this.copy(numTmp = num, tupleTmp = tt)

    def contains(termName: TermName): Boolean =
      vals.contains(termName) || funcs.contains(termName)
    def contains(tree: Tree): Boolean = {
      tree match {
        case Select(This(this.name), termName: TermName) => contains(termName)
        case _ =>
          unprocessedTree(tree, "CircuitInfo.contains")
          false
      }
    }

    def getCType(tree: Tree): CType = {
      val tpe = getMType(tree)
      tpe match {
        case c: CType => c
        case _ =>
          reporter.error(tree.pos, "Not CType")
          CType.empty
      }
    }

    def getMType(tree: Tree): MType = {
      def select(tpe: CType, termName: TermName): MType = tpe match {
        case Bundle(physical, signals) if signals.contains(termName) =>
          signals(termName)
        case _ => {
          reporter.error(tree.pos, s"TermName ${termName} not found in ${tpe}")
          CType.empty
        }
      }
      tree match {
        case Select(This(this.name), termName: TermName) => vals(termName)
        case Select(qualifier, termName: TermName)       => select(getCType(qualifier), termName)
        case Ident(termName: TermName)                   => vals(termName)
        case _ => {
          unprocessedTree(tree, "CircuitInfo.getCType")
          reporter.error(tree.pos, s"CircuitInfo.getCType not process")
          CType.empty
        }
      }
    }

    def getFunctionInfo(tree: Tree): MType = {
      tree match {
        case Select(This(this.name), termName: TermName) => funcs(termName)
      }
    }
  }

  object CircuitInfo {
    def apply(name: TypeName) = new CircuitInfo(name, List.empty, Map.empty, Map.empty, 0, None, None)

    def empty = new CircuitInfo(TypeName(""), List.empty, Map.empty, Map.empty, 0, None, None)
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
