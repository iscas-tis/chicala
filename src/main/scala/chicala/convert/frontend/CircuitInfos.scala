package chicala.convert.frontend

import scala.tools.nsc.Global

trait CircuitInfos { self: Scala2Reader =>
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
      val tupleTmp: Option[(TermName, SUnapplyDef)],
      /* info from compile plugin global*/
      val readerInfo: ReaderInfo
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

    def getSignalType(tree: Tree): SignalType = {
      val tpe = getMType(tree)
      tpe match {
        case c: SignalType => c
        case _ =>
          reporter.error(tree.pos, "Not SignalType")
          SignalType.empty
      }
    }

    def getMType(tree: Tree): MType = {
      def select(tpe: SignalType, termName: TermName): MType = tpe match {
        case Bundle(physical, signals) if signals.contains(termName) =>
          signals(termName)
        case _ => {
          reporter.error(tree.pos, s"TermName ${termName} not found in ${tpe}")
          SignalType.empty
        }
      }
      tree match {
        case Select(This(this.name), termName: TermName) => vals(termName)
        case Select(qualifier, termName: TermName)       => select(getSignalType(qualifier), termName)
        case Ident(termName: TermName)                   => vals(termName)
        case _ => {
          unprocessedTree(tree, "CircuitInfo.getSignalType")
          reporter.error(tree.pos, s"CircuitInfo.getSignalType not process")
          SignalType.empty
        }
      }
    }

    def getFunctionInfo(tree: Tree): MType = {
      tree match {
        case Select(This(this.name), termName: TermName) => funcs(termName)
      }
    }

    def settedDependentClassNotDef = copy(readerInfo = readerInfo.settedDependentClassNotDef)
    def isDependentClassNotDef     = readerInfo.isDependentClassNotDef
    def needExit                   = readerInfo.needExit
  }

  object CircuitInfo {
    def apply(name: TypeName)(implicit readerInfo: ReaderInfo) =
      new CircuitInfo(name, List.empty, Map.empty, Map.empty, 0, None, None, readerInfo)

    def empty(implicit readerInfo: ReaderInfo) =
      new CircuitInfo(TypeName(""), List.empty, Map.empty, Map.empty, 0, None, None, readerInfo)
  }

}
