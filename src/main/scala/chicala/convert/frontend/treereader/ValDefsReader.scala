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
        case v @ ValDef(mods, nameTmp, tpt, rhs) if isChiselType(tpt) => {
          // SignalDef
          val name = nameTmp.stripSuffix(" ")
          passThrough(rhs)._1 match {
            case a @ Apply(func, args) =>
              if (isModuleThisIO(func, cInfo)) {
                // IoDef
                val someBundleDef = args.head match {
                  case Block(stats, expr) => BundleDefLoader(stats.head)
                  case _                  => None
                }

                val bundle  = someBundleDef.get.bundle.updatedPhysical(Io)
                val newInfo = cInfo.updatedSignal(name, bundle)
                val ioDef   = IoDef(name, bundle)

                Some((newInfo, Some(ioDef)))
              } else if (isChisel3WireApply(func)) {
                Some(loadWireDef(cInfo, name, func, args))
              } else if (isChiselRegDefApply(func)) {
                Some(loadRegDef(cInfo, name, func, args))
              } else {
                // NodeDef
                val cExp       = MTermLoader(cInfo, rhs).get._2.get
                val signalInfo = cExp.tpe.asInstanceOf[CType].updatedPhysical(Node)
                val newInfo    = cInfo.updatedSignal(name, signalInfo)
                Some((newInfo, Some(NodeDef(name, signalInfo, cExp))))
              }
            case s @ Select(Select(This(typeName), termname), _)
                if cInfo.enumTmp.nonEmpty &&
                  typeName == cInfo.name && termname == cInfo.enumTmp.get._1 =>
              // EnumDef step 2
              val (tn, ed) = cInfo.enumTmp.get
              val num      = cInfo.numTmp - 1
              val enumDef  = EnumDef(ed.names :+ name, ed.info)
              val enumTmp  = (tn, enumDef)
              val newCInfo = cInfo.updatedSignal(name, enumDef.info)
              if (num == 0)
                Some((newCInfo.updatedEnumTmp(0, None), Some(enumTmp._2)))
              else
                Some((newCInfo.updatedEnumTmp(num, Some(enumTmp)), None))
            case s @ Select(Select(This(typeName), termname), _)
                if cInfo.tupleTmp.nonEmpty &&
                  typeName == cInfo.name && termname == cInfo.tupleTmp.get._1 =>
              // STupleUnapplyDef step 2
              val (tn, stud)       = cInfo.tupleTmp.get
              val num              = cInfo.numTmp - 1
              val sTupleUnapplyDef = stud.copy(names = stud.names :+ name)
              val tupleTmp         = (tn, sTupleUnapplyDef)
              val newCInfo =
                if (isChiselType(tpt))
                  cInfo.updatedSignal(name, CTypeLoader.fromTypeTree(tpt))
                else
                  cInfo.updatedParam(name, tpt.asInstanceOf[TypeTree])
              if (num == 0)
                Some((newCInfo.updatedTupleTmp(0, None), Some(tupleTmp._2)))
              else
                Some((newCInfo.updatedTupleTmp(num, Some(tupleTmp)), None))
            case EmptyTree =>
              val tpe      = CTypeLoader(tpt)
              val newCInfo = cInfo.updatedSignal(name, tpe)
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
                  UInt(Node, Undirect) // width: Literal(Constant(BigInt(num - 1).bitLength))
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
          Some(
            (
              cInfo.updatedTupleTmp(
                num,
                Some((name, SUnapplyDef(List.empty, cExp, tpt.tpe.typeArgs)))
              ),
              None
            )
          )
        }
        case ValDef(mods, name, tpt, rhs) =>
          val newCInfo =
            if (isChiselType(tpt))
              cInfo.updatedSignal(name, CTypeLoader(tpt))
            else
              cInfo.updatedParam(name, tpt.asInstanceOf[TypeTree])
          val tpe = STypeLoader(tpt)
          val r = MTermLoader(cInfo, rhs) match {
            case Some((_, Some(value))) => value
            case _                      => EmptyMTerm
          }
          Some((newCInfo, Some(SValDef(name, tpe, r))))
        case _ =>
          unprocessedTree(tr, "ValDefReader")
          None
      }

    }

    def loadWireDef(
        cInfo: CircuitInfo,
        name: TermName,
        func: Tree,
        args: List[Tree]
    ): (CircuitInfo, Option[CValDef]) = {
      assert(args.length == 1, "should have only 1 arg in Wire()")

      val cType   = CTypeLoader(args.head).updatedPhysical(Wire)
      val newInfo = cInfo.updatedSignal(name, cType)
      (newInfo, Some(WireDef(name, cType)))
    }
    def loadRegDef(
        cInfo: CircuitInfo,
        name: TermName,
        func: Tree,
        args: List[Tree]
    ): (CircuitInfo, Option[CValDef]) = {
      if (isChisel3RegApply(func)) {
        assert(args.length == 1, "should have only 1 arg in Reg()")

        val signalInfo = CTypeLoader(args.head).updatedPhysical(Reg)
        val newCInfo   = cInfo.updatedSignal(name, signalInfo)
        (newCInfo, Some(RegDef(name, signalInfo)))

      } else if (isChisel3RegInitApply(func)) {
        if (args.length == 1) {
          val init       = MTermLoader(cInfo, args.head).get._2.get
          val signalInfo = init.tpe.asInstanceOf[CType].updatedPhysical(Reg)
          val newCInfo   = cInfo.updatedSignal(name, signalInfo)
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
        val signalInfo = next.tpe.asInstanceOf[CType]
        val newCInfo   = cInfo.updatedSignal(name, signalInfo)
        (newCInfo, Some(RegDef(name, signalInfo, None, Some(next), Some(enable))))
      } else {
        reporter.error(func.pos, "Unknow RegDef function")
        (cInfo, None)
      }
    }
  }

}
