package chicala.convert.frontend

import scala.tools.nsc.Global

trait MStatementsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  trait MStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MStatement])]
  }

  object MStatementLoader extends MStatementObj {
    private def someStatementIn(
        cInfo: CircuitInfo,
        tree: Tree,
        objs: List[MStatementObj]
    ): Option[(CircuitInfo, Option[MStatement])] = {
      val ls = objs.map(_.apply(cInfo, tree)).flatten

      assert(ls.length <= 1, "should be at most one statement")
      ls match {
        case head :: next => Some(head)
        case Nil => {
          unprocessedTree(tree, "MStatement.someStatementIn")
          None
        }
      }
    }

    def fromListTree(cInfo: CircuitInfo, body: List[Tree]): (CircuitInfo, List[MStatement]) = {
      (body.foldLeft((cInfo, List.empty[MStatement])) { case ((info, past), tr) =>
        apply(info, tr) match {
          case Some((newInfo, Some(newStat))) => (newInfo, newStat :: past)
          case Some((newInfo, None))          => (newInfo, past)
          case None                           => (info, past)
        }
      }) match { case (info, past) => (info, past.reverse) }
    }

    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MStatement])] = {
      val (tree, someTpe) = passThrough(tr)
      tree match {
        case v @ ValDef(mods, name, tpt, rhs) => {
          if (mods.isParamAccessor) None // pass ParamAccessor
          else ValDefLoader(cInfo, tree)
        }
        case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
          name match {
            // constructor of this class
            case termNames.CONSTRUCTOR => {
              // register construct param
              val params = vparamss.flatten.map(x => (x.name, x.tpt.asInstanceOf[TypeTree]))
              Some((cInfo.updatedParams(params), None))
            }
            // accessor of signal
            case t: TermName if cInfo.signal.contains(t) => None
            case _ =>
              SDefDefLoader(cInfo, tree) match {
                case Some(value) => Some(value)
                case None =>
                  unprocessedTree(tree, "MStatementLoader")
                  None
              }
          }
        }
        case a: Apply => {
          someStatementIn(cInfo, tree, List(AssertLoader, WhenLoader, ConnectLoader)) match {
            case Some(value) => Some(value)
            case None        => SApplyLoader(cInfo, a)
          }
        }
        // empty statement
        case Literal(Constant(())) =>
          None
        case _ => {
          unprocessedTree(tree, "MStatementLoader case _")
          None
        }
      }
    }

  }

  object ValDefLoader extends MStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MStatement])] = {
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
                val cExp       = CExpLoader(cInfo, rhs)
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
            case _ =>
              // ? TODO?
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
            if (isScala2TupleUnapplyTmpValDef(v)) CExpLoader(cInfo, args.head)
            else CExpLoader(cInfo, t)
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
        case v: ValDef =>
          SValDefLoader(cInfo, tr)
        case _ => None
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
          val init       = CExpLoader(cInfo, args.head)
          val signalInfo = init.tpe.asInstanceOf[CType].updatedPhysical(Reg)
          val newCInfo   = cInfo.updatedSignal(name, signalInfo)
          (newCInfo, Some(RegDef(name, signalInfo, Some(init))))
        } else {
          unprocessedTree(func, s"ValDefLoader.loadRegDef with ${args.size} arg")
          (cInfo, None)
        }
      } else if (isChisel3UtilRegEnableApply(func)) {
        if (args.length != 2)
          reporter.error(func.pos, "should have 2 args in RegEnable()")
        val next       = CExpLoader(cInfo, args.head)
        val enable     = CExpLoader(cInfo, args.tail.head)
        val signalInfo = next.tpe.asInstanceOf[CType]
        val newCInfo   = cInfo.updatedSignal(name, signalInfo)
        (newCInfo, Some(RegDef(name, signalInfo, None, Some(next), Some(enable))))
      } else {
        reporter.error(func.pos, "Unknow RegDef function")
        (cInfo, None)
      }
    }

  }

  object ConnectLoader extends MStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Connect])] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Select(termName: Select, TermName("$colon$eq")), args) =>
          assert(args.length == 1, "should have one right expr")
          val left  = CExpLoader(cInfo, termName)
          val right = CExpLoader(cInfo, args.head)
          left match {
            case x: SignalRef => Some(cInfo, Some(Connect(x, right)))
            case _ =>
              unprocessedTree(termName, "ConnectLoader")
              None
          }
        case _ => None
      }
    }
  }
  object WhenLoader extends MStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[When])] = {
      def bodyFromTree(cInfo: CircuitInfo, tr: Tree): List[MStatement] = {
        val (tree, _) = passThrough(tr)
        val treeBody = tree match {
          case Block(stats, expr) => stats :+ expr
          case tr                 => List(tr)
        }
        MStatementLoader.fromListTree(cInfo, treeBody)._2
      }
      def pushBackElseWhen(when: When, elseWhen: When): When = when match {
        case When(cond, whenBody, otherBody, true) =>
          When(
            cond,
            whenBody,
            List(pushBackElseWhen(otherBody.head.asInstanceOf[When], elseWhen)),
            true
          )
        case When(cond, whenBody, otherBody, false) =>
          When(cond, whenBody, List(elseWhen), true)
      }

      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Apply(cwa, condArgs), args) if isChisel3WhenApply(cwa) => {
          val cond     = CExpLoader(cInfo, condArgs.head)
          val whenBody = bodyFromTree(cInfo, args.head)
          Some((cInfo, Some(When(cond, whenBody, List.empty))))
        }
        case Apply(Select(qualifier, TermName("otherwise")), args) => {
          val Some((newCInfo, Some(when))) = WhenLoader(cInfo, qualifier)
          val otherBody                    = bodyFromTree(cInfo, args.head)
          Some((cInfo, Some(When(when.cond, when.whenBody, otherBody))))
        }
        case Apply(Apply(Select(qualifier, TermName("elsewhen")), condArgs), args) => {
          val Some((newCInfo, Some(when))) = WhenLoader(cInfo, qualifier)

          val elseCond     = CExpLoader(cInfo, condArgs.head)
          val elseWhen     = When(elseCond, bodyFromTree(cInfo, args.head), List.empty)
          val whenElseWhen = pushBackElseWhen(when, elseWhen)

          Some((cInfo, Some(whenElseWhen)))
        }
        case _ => None
      }
    }
  }

  object AssertLoader extends MStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Assert])] = {
      val (tree, _) = passThrough(tr)
      if (isReturnAssert(tree)) {
        tree match {
          case Apply(Ident(TermName("_applyWithSourceLinePrintable")), args) =>
            Some(cInfo, Some(Assert(CExpLoader(cInfo, args.head))))
          case _ => None
        }
      } else None
    }
  }

}
