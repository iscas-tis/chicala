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
      this.copy(vals = vals + (strippedName(termName) -> tpe))

    def updatedFunc(termName: TermName, tpe: MType): CircuitInfo =
      this.copy(funcs = funcs + (strippedName(termName) -> tpe))

    def updatedEnumTmp(num: Int, et: Option[(TermName, EnumDef)]): CircuitInfo = {
      val newET = et.map { case (name, enumDef) => (strippedName(name), enumDef) }
      this.copy(numTmp = num, enumTmp = newET)
    }
    def updatedTupleTmp(num: Int, tt: Option[(TermName, SUnapplyDef)]): CircuitInfo = {
      val newTT = tt.map { case (name, sUnapplyDef) => (strippedName(name), sUnapplyDef) }
      this.copy(numTmp = num, tupleTmp = newTT)
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
        case Select(s @ Select(This(this.name), termName: TermName), TermName("io")) if isChiselModuleType(s) =>
          val tpe       = vals(termName).asInstanceOf[SubModule]
          val moduleDef = readerInfo.moduleDefs(tpe.fullName)
          val ioDefs = moduleDef.body
            .filter({
              case IoDef(name, tpe) => true
              case _                => false
            })
          assert(ioDefs.tail == Nil, "ModuleDef should has only one IoDef")
          ioDefs.head.tpe
        case Select(qualifier, termName: TermName) => select(getSignalType(qualifier), termName)
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
