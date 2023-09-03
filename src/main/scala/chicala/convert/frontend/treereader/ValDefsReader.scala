package chicala.convert.frontend

import scala.tools.nsc.Global
import chicala.ast.impl.MTermImpls

trait ValDefsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object ValDefReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MDef])] = {
      val tree = passThrough(tr)._1
      tree match {
        // SignalDef and SubModuleDef
        case v @ ValDef(mods, nameTmp, tpt: TypeTree, rhs) if isChiselSignalType(tpt) || isChiselModuleType(tpt) => {
          val name = nameTmp.stripSuffix(" ")
          passThrough(rhs)._1 match {
            // normal SignalDef and SubModuleDef
            case a @ Apply(func, args) =>
              if (isModuleThisIO(func, cInfo)) { // IoDef
                Some(loadIoDef(cInfo, name, args))
              } else if (isChiselWireDefApply(func)) { // WireDef
                Some(loadWireDef(cInfo, name, func, args, mods.isMutable))
              } else if (isChiselRegDefApply(func)) { // RegDef
                Some(loadRegDef(cInfo, name, func, args))
              } else if (isChisel3ModuleDoApply(func)) { // SubModuleDef
                Some(loadSubModuleDef(cInfo, name, args))
              } else { // NodeDef called function or operator
                Some(loadNodeDef(cInfo, name, rhs))
              }
            // EnumDef step 2
            case s @ Select(Select(This(typeName), termname), _)
                if cInfo.enumTmp.nonEmpty &&
                  typeName == cInfo.name && termname == cInfo.enumTmp.get._1 => {
              val (tn, ed) = cInfo.enumTmp.get
              val num      = cInfo.numTmp - 1
              val enumDef  = EnumDef(ed.names :+ name, ed.tpe)
              val enumTmp  = (tn, enumDef)
              val newCInfo = cInfo.updatedVal(name, enumDef.tpe)
              if (num == 0)
                Some((newCInfo.updatedEnumTmp(0, None), Some(enumTmp._2)))
              else
                Some((newCInfo.updatedEnumTmp(num, Some(enumTmp)), None))
            }
            // STupleUnapplyDef step 2
            case s @ Select(Select(This(typeName), termname), _)
                if cInfo.tupleTmp.nonEmpty &&
                  typeName == cInfo.name && termname == cInfo.tupleTmp.get._1 =>
              val (tn, stud)       = cInfo.tupleTmp.get
              val num              = cInfo.numTmp - 1
              val sTupleUnapplyDef = stud.copy(names = stud.names :+ name)
              val tupleTmp         = (tn, sTupleUnapplyDef)
              val newCInfo         = cInfo.updatedVal(name, MTypeLoader.fromTpt(tpt).get)
              if (num == 0)
                Some((newCInfo.updatedTupleTmp(0, None), Some(tupleTmp._2)))
              else
                Some((newCInfo.updatedTupleTmp(num, Some(tupleTmp)), None))
            case EmptyTree =>
              val tpe      = SignalTypeLoader.fromTpt(tpt).get
              val newCInfo = cInfo.updatedVal(name, tpe)
              val nodeDef  = NodeDef(name, tpe, EmptyMTerm)
              Some((newCInfo, Some(nodeDef)))
            case _ =>
              Some(loadNodeDef(cInfo, name, rhs))
          }
        }
        // EnumDef step 1
        case v @ ValDef(mods, name, tpt, rhs) if isChisel3EnumTmpValDef(v) => {
          rhs match {
            case Match(Typed(Apply(cuea, number), _), _) => {
              val num = number.head.asInstanceOf[Literal].value.value.asInstanceOf[Int]
              val enumTmp = (
                name,
                EnumDef(
                  List.empty,
                  UInt(
                    KnownSize(SLiteral(BigInt(num - 1).bitLength, StInt)),
                    Node,
                    Undirect
                  ) // width: Literal(Constant(BigInt(num - 1).bitLength))
                )
              )
              Some((cInfo.updatedEnumTmp(num, Some(enumTmp)), None))
            }
          }
        }
        // STupleUnapplyDef step 1
        case v @ ValDef(mods, name, tpt, Match(t @ Typed(rhs, _), _)) => {
          val num  = tpt.tpe.typeArgs.length
          val cExp = MTermLoader(cInfo, rhs).get._2.get
          val tpe  = STypeLoader.fromTpt(tpt).get.asInstanceOf[StTuple]
          Some(
            (
              cInfo.updatedTupleTmp(
                num,
                Some((name, SUnapplyDef(List.empty, cExp, tpe)))
              ),
              None
            )
          )
        }
        // SValDef
        case ValDef(mods, nameTmp, tpt, rhs) =>
          val name     = nameTmp.stripSuffix(" ")
          val tpe      = STypeLoader.fromTpt(tpt).get
          val newCInfo = cInfo.updatedVal(name, tpe)
          val r = MTermLoader(cInfo, rhs) match {
            case Some((_, Some(value))) => value
            case _                      => EmptyMTerm
          }
          val sValDef = SValDef(name, tpe, r)
          if (mods.isParamAccessor) {
            if (mods.isParameter)
              Some((newCInfo, Some(sValDef)))
            else
              Some((newCInfo, None))
          } else
            Some((newCInfo, Some(sValDef)))
        case _ =>
          unprocessedTree(tr, "ValDefReader")
          None
      }

    }

    def loadIoDef(
        cInfo: CircuitInfo,
        name: TermName,
        args: List[Tree]
    ): (CircuitInfo, Option[IoDef]) = {
      val someInfoAndDef = args.head match {
        case Block(stats, expr) => BundleDefLoader(cInfo, stats.head, "")
        case a @ Apply(Select(New(tpt), termNames.CONSTRUCTOR), aparams) =>
          val bundleFullName = tpt.tpe.toString()
          val someBundleDef  = cInfo.readerInfo.bundleDefs.get(bundleFullName)
          val mArgs          = aparams.map(MTermLoader(cInfo, _).get._2.get)
          someBundleDef match {
            case Some(bundleDef) => Some((cInfo, Some(bundleDef.applyArgs(mArgs))))
            case None            => Some((cInfo.settedDependentClassNotDef, None))
          }
        case _ => None // this should not happed
      }
      val (tmpCInfo, someBundleDef) = someInfoAndDef.get

      someBundleDef match {
        case None => (tmpCInfo, None)
        case Some(bundleDef) =>
          val bundle  = bundleDef.bundle.updatedPhysical(Io)
          val newInfo = tmpCInfo.updatedVal(name, bundle)
          val ioDef   = IoDef(name, bundle)

          (newInfo, Some(ioDef))
      }
    }

    def loadWireDef(
        cInfo: CircuitInfo,
        name: TermName,
        func: Tree,
        args: List[Tree],
        isVar: Boolean
    ): (CircuitInfo, Option[WireDef]) = {
      if (isChisel3WireApply(func)) {
        assertError(args.length == 1, func.pos, "Should have only 1 arg in Wire()")
        val sigType = SignalTypeLoader(cInfo, args.head).get.updatedPhysical(Wire)
        val newInfo = cInfo.updatedVal(name, sigType)
        (newInfo, Some(WireDef(name, sigType)))
      } else if (isChisel3WireInitApply(func)) {
        val init    = MTermLoader(cInfo, args.head).get._2.get
        val sigType = init.tpe.asInstanceOf[SignalType].updatedPhysical(Wire)
        val newInfo = cInfo.updatedVal(name, sigType)
        // isVar onlay support WireInit now
        val wireDef = WireDef(name, sigType, Some(init), isVar)
        (newInfo, Some(wireDef))
      } else if (isChisel3VecInitDoApply(func)) {
        assertError(args.length >= 1, func.pos, "Should have at last 1 arg in VecInit()")
        val mArgs = args.map(MTermLoader(cInfo, _).get._2.get)
        val init  = STuple(mArgs, StTuple(mArgs.map(_.tpe)))
        val tpe = Vec(
          KnownSize.fromInt(init.size),
          Wire,
          init.tpe.tparams.head.asInstanceOf[SignalType]
        )
        val newInfo = cInfo.updatedVal(name, tpe)
        (newInfo, Some(WireDef(name, tpe, Some(init))))
      } else {
        reporter.error(func.pos, "Unknow WireDef function")
        (cInfo, None)
      }

    }
    def loadRegDef(
        cInfo: CircuitInfo,
        name: TermName,
        func: Tree,
        args: List[Tree]
    ): (CircuitInfo, Option[RegDef]) = {
      if (isChisel3RegApply(func)) {
        assert(args.length == 1, "should have only 1 arg in Reg()")

        val signalInfo = SignalTypeLoader(cInfo, args.head).get.updatedPhysical(Reg)
        val newCInfo   = cInfo.updatedVal(name, signalInfo)
        (newCInfo, Some(RegDef(name, signalInfo)))

      } else if (isChisel3RegInitApply(func)) {
        if (args.length == 1) {
          val init       = MTermLoader(cInfo, args.head).get._2.get
          val signalInfo = init.tpe.asInstanceOf[SignalType].updatedPhysical(Reg)
          val newCInfo   = cInfo.updatedVal(name, signalInfo)
          (newCInfo, Some(RegDef(name, signalInfo, Some(init))))
        } else {
          unprocessedTree(func, s"ValDefReader.loadRegDef with ${args.size} arg")
          (cInfo, None)
        }
      } else if (isChisel3UtilRegEnableApply(func)) {
        if (args.length != 2)
          reporter.error(func.pos, "should have 2 args in RegEnable()")
        val next       = MTermLoader(cInfo, args.head).get._2.get
        val enable     = MTermLoader(cInfo, args.tail.head).get._2.get
        val signalInfo = next.tpe.asInstanceOf[SignalType].updatedPhysical(Reg)
        val newCInfo   = cInfo.updatedVal(name, signalInfo)
        (newCInfo, Some(RegDef(name, signalInfo, None, Some(next), Some(enable))))
      } else {
        reporter.error(func.pos, "Unknow RegDef function")
        (cInfo, None)
      }
    }

    def loadNodeDef(
        cInfo: CircuitInfo,
        name: TermName,
        rhs: Tree
    ): (CircuitInfo, Option[NodeDef]) = {
      val cExp       = MTermLoader(cInfo, rhs).get._2.get
      val signalInfo = cExp.tpe.asInstanceOf[SignalType].updatedPhysical(Node)
      val newInfo    = cInfo.updatedVal(name, signalInfo)
      (newInfo, Some(NodeDef(name, signalInfo, cExp)))
    }

    def loadSubModuleDef(
        cInfo: CircuitInfo,
        name: TermName,
        args: List[Tree]
    ): (CircuitInfo, Option[SubModuleDef]) = {
      args.head match {
        case Apply(Select(New(tpt), termNames.CONSTRUCTOR), args) =>
          val moduleFullName = tpt.tpe.toString()
          val someModuleDef  = cInfo.readerInfo.moduleDefs.get(moduleFullName)
          someModuleDef match {
            case Some(value) =>
              val tpe          = SubModule(moduleFullName)
              val subModuleDef = SubModuleDef(name, tpe, args.map(x => MTermLoader(cInfo, x).get._2.get))
              (cInfo.updatedVal(name, tpe), Some(subModuleDef))
            case None =>
              (cInfo.settedDependentClassNotDef, None)
          }
        case _ => (cInfo, None)
      }
    }

  }

}
