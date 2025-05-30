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
      val termNameTmp: Option[TermName],
      val enumDefTmp: Option[EnumDef],
      val sUnapplyDefTmp: Option[SUnapplyDef],
      /* info from compile plugin global*/
      val readerInfo: ReaderInfo
  ) {

    def updatedParams(sValDefs: List[SValDef]): CircuitInfo =
      this.copy(params = sValDefs)

    def updatedVal(termName: TermName, tpe: MType): CircuitInfo =
      this.copy(vals = vals + (strippedName(termName) -> tpe))

    def updatedFunc(termName: TermName, tpe: MType): CircuitInfo =
      this.copy(funcs = funcs + (strippedName(termName) -> tpe))

    def updatedEnumDefTmp(num: Int, termName: Option[TermName], ed: Option[EnumDef]): CircuitInfo = {
      if (num > 0) {
        val name = termName.map(strippedName(_))
        this.copy(numTmp = num, termNameTmp = name, enumDefTmp = ed)
      } else {
        this.copy(numTmp = 0, termNameTmp = None, enumDefTmp = None, sUnapplyDefTmp = None)
      }
    }
    def updatedSUnapplyDefTmp(num: Int, termName: Option[TermName], sud: Option[SUnapplyDef]): CircuitInfo = {
      if (num > 0) {
        val name = termName.map(strippedName(_))
        this.copy(numTmp = num, termNameTmp = name, sUnapplyDefTmp = sud)
      } else {
        this.copy(numTmp = num, termNameTmp = None, enumDefTmp = None, sUnapplyDefTmp = None)
      }
    }

    def contains(termName: TermName): Boolean = {
      val name = strippedName(termName)
      vals.contains(termName) || funcs.contains(name)
    }
    def contains(tree: Tree): Boolean = {
      tree match {
        case Select(This(this.name), termName: TermName) => contains(termName)
        case _ =>
          unprocessedTree(tree, "CircuitInfo.contains")
          false
      }
    }

    def getVal(name: TermName): Option[MType] = {
      Some(vals(strippedName(name)))
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
        case Ident(termName: TermName)                   => getVal(termName).get
        case Select(This(this.name), termName: TermName) => getVal(termName).get
        case Select(qualifier, TermName("io")) if isChiselModuleType(qualifier) =>
          val termName = qualifier match {
            case Select(This(this.name), name: TermName) => name
            case Ident(name: TermName)                   => name
            case _ =>
              reportError(qualifier.pos, "Unknown structure in CircuitInfo.getMType #1")
              TermName("")
          }
          val tpe       = vals(termName).asInstanceOf[SubModule]
          val moduleDef = readerInfo.moduleDefs(tpe.fullName)
          moduleDef.ioDef.tpe
        case Select(qualifier, termName: TermName) => select(getSignalType(qualifier), termName)
        case _ => {
          reportError(tree.pos, "Unknown structure in CircuitInfo.getMType #2")
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
      new CircuitInfo(name, List.empty, Map.empty, Map.empty, 0, None, None, None, readerInfo)

    def empty(implicit readerInfo: ReaderInfo) =
      new CircuitInfo(TypeName(""), List.empty, Map.empty, Map.empty, 0, None, None, None, readerInfo)
  }

}
