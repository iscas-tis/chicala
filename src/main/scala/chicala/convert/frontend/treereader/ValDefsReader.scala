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
        // SignalDef
        case v @ ValDef(mods, nameTmp, tpt: TypeTree, rhs) if isChiselType(tpt) => {
          val name = nameTmp.stripSuffix(" ")
          passThrough(rhs)._1 match {
            // normal SignalDef
            case a @ Apply(func, args) =>
              if (isModuleThisIO(func, cInfo)) { // IoDef
                Some(loadIoDef(cInfo, name, args))
              } else if (isChiselWireDefApply(func)) { // WireDef
                Some(loadWireDef(cInfo, name, func, args))
              } else if (isChiselRegDefApply(func)) { // RegDef
                Some(loadRegDef(cInfo, name, func, args))
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
              val newCInfo         = cInfo.updatedVal(name, MTypeLoader(tpt).get)
              if (num == 0)
                Some((newCInfo.updatedTupleTmp(0, None), Some(tupleTmp._2)))
              else
                Some((newCInfo.updatedTupleTmp(num, Some(tupleTmp)), None))
            case s: Select => Some(loadNodeDef(cInfo, name, rhs))
            case EmptyTree =>
              val tpe      = CTypeLoader(tpt).get
              val newCInfo = cInfo.updatedVal(name, tpe)
              val nodeDef  = NodeDef(name, tpe, EmptyMTerm)
              Some((newCInfo, Some(nodeDef)))
            case _ =>
              None
          }
        }
        case v @ ValDef(mods, name, tpt, rhs) if isChisel3EnumTmpValDef(v) => {
          // EnumDef step 1
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
        case v @ ValDef(mods, name, tpt, Match(t @ Typed(Apply(appl, args), _), _)) => {
          // STupleUnapplyDef step 1
          val num = tpt.tpe.typeArgs.length
          val cExp =
            if (isScala2TupleUnapplyTmpValDef(v)) MTermLoader(cInfo, args.head).get._2.get
            else MTermLoader(cInfo, t).get._2.get // ? what is this ?
          val tpe = STypeLoader(tpt).get.asInstanceOf[StTuple]
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
        case ValDef(mods, name, tpt, rhs) =>
          // SValDef
          val tpe      = STypeLoader(tpt).get
          val newCInfo = cInfo.updatedVal(name, tpe)
          val r = MTermLoader(cInfo, rhs) match {
            case Some((_, Some(value))) => value
            case _                      => EmptyMTerm
          }
          val sValDef = SValDef(name, tpe, r)
          if (mods.isParamAccessor) Some((newCInfo.updatedParam(sValDef), None))
          else Some((newCInfo, Some(sValDef)))
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
        case Block(stats, expr) => BundleDefLoader(cInfo, stats.head)
        case a @ Apply(Select(New(tpt), termNames.CONSTRUCTOR), aparams) =>
          val className     = tpt.toString()
          val someBundleDef = cInfo.readerInfo.bundleDefs.get(className)
          someBundleDef match {
            case Some(bundleDef) => Some((cInfo, Some(bundleDef)))
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
        args: List[Tree]
    ): (CircuitInfo, Option[WireDef]) = {
      assert(args.length == 1, "should have only 1 arg in Wire()")
      if (isChisel3WireApply(func)) {
        val cType   = CTypeLoader(cInfo, args.head).get.updatedPhysical(Wire)
        val newInfo = cInfo.updatedVal(name, cType)
        (newInfo, Some(WireDef(name, cType)))
      } else if (isChisel3WireInitApply(func)) {
        val init    = MTermLoader(cInfo, args.head).get._2.get
        val cType   = init.tpe.asInstanceOf[CType].updatedPhysical(Wire)
        val newInfo = cInfo.updatedVal(name, cType)
        (newInfo, Some(WireDef(name, cType, Some(init))))
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

        val signalInfo = CTypeLoader(cInfo, args.head).get.updatedPhysical(Reg)
        val newCInfo   = cInfo.updatedVal(name, signalInfo)
        (newCInfo, Some(RegDef(name, signalInfo)))

      } else if (isChisel3RegInitApply(func)) {
        if (args.length == 1) {
          val init       = MTermLoader(cInfo, args.head).get._2.get
          val signalInfo = init.tpe.asInstanceOf[CType].updatedPhysical(Reg)
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
        val signalInfo = next.tpe.asInstanceOf[CType].updatedPhysical(Reg)
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
      val signalInfo = cExp.tpe.asInstanceOf[CType].updatedPhysical(Node)
      val newInfo    = cInfo.updatedVal(name, signalInfo)
      (newInfo, Some(NodeDef(name, signalInfo, cExp)))
    }
  }

}
