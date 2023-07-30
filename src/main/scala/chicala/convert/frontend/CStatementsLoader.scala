package chicala.convert.frontend

import scala.tools.nsc.Global

trait CStatementsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  trait CStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[CStatement])]
  }

  object CStatementLoader extends CStatementObj {
    private def someStatementIn(
        cInfo: CircuitInfo,
        tree: Tree,
        objs: List[CStatementObj]
    ): Option[(CircuitInfo, Option[CStatement])] = {
      val ls = objs.map(_.apply(cInfo, tree)).flatten

      assert(ls.length <= 1, "should be at most one statement")
      ls match {
        case head :: next => Some(head)
        case Nil => {
          unprocessedTree(tree, "CStatement.someStatementIn")
          None
        }
      }
    }

    def fromListTree(cInfo: CircuitInfo, body: List[Tree]): (CircuitInfo, List[CStatement]) = {
      (body.foldLeft((cInfo, List.empty[CStatement])) { case ((info, past), tr) =>
        apply(info, tr) match {
          case Some((newInfo, Some(newStat))) => (newInfo, newStat :: past)
          case Some((newInfo, None))          => (newInfo, past)
          case None                           => (info, past)
        }
      }) match { case (info, past) => (info, past.reverse) }
    }

    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[CStatement])] = {
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
                  unprocessedTree(tree, "CStatementLoader")
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
          unprocessedTree(tree, "CStatementLoader case _")
          None
        }
      }
    }

  }

  object ValDefLoader extends CStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[CStatement])] = {
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

                val bundle  = someBundleDef.get.bundle
                val newInfo = cInfo.updatedSignal(name, SignalInfo(Io, bundle))
                val ioDef   = IoDef(name, SignalInfo(Io, bundle))

                Some((newInfo, Some(ioDef)))
              } else if (isChisel3WireApply(func)) {
                // WireDef
                assert(args.length == 1, "should have only 1 arg in Wire()")

                val dataType   = CDataTypeLoader(args.head)
                val signalInfo = SignalInfo(Wire, dataType)
                val newInfo    = cInfo.updatedSignal(name, signalInfo)
                Some(newInfo, Some(WireDef(name, signalInfo)))
              } else {
                // SymbolDef
                val cExp       = CExpLoader(cInfo, rhs)
                val signalInfo = SignalInfo(Symbol, cExp.info.dataType)
                val newInfo    = cInfo.updatedSignal(name, signalInfo)
                Some((newInfo, Some(SymbolDef(name, signalInfo, cExp))))
              } // TODO: RegDef
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
                  cInfo.updatedSignal(name, SignalInfo(Symbol, CDataTypeLoader.fromTreeTpe(tpt)))
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
                  SignalInfo(
                    Node,
                    UInt(Literal(Constant(BigInt(num - 1).bitLength)), Undirect)
                  )
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
                Some((name, STupleUnapplyDef(List.empty, cExp, tpt.tpe.typeArgs)))
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
  }

  object WireDefLoader extends CStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[WireDef])] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case ValDef(mods, nameTmp, tpt, apply) =>
          passThrough(apply)._1 match {
            case Apply(
                  TypeApply(Select(Select(Ident(TermName("chisel3")), TermName("Wire")), TermName("apply")), _),
                  args
                ) =>
              assert(args.length == 1, "should have only 1 arg in Wire()")

              val name       = nameTmp.stripSuffix(" ")
              val dataType   = CDataTypeLoader(args.head)
              val signalInfo = SignalInfo(Wire, dataType)
              val newInfo    = cInfo.updatedSignal(name, signalInfo)
              Some(newInfo, Some(WireDef(name, signalInfo)))
            case _ => None
          }
        case _ => None
      }

    }
  }

  object ConnectLoader extends CStatementObj {
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
  object WhenLoader extends CStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[When])] = {
      def whenFromTree(cInfo: CircuitInfo, tr: Tree): (CExp, List[CStatement]) = {
        val (tree, _) = passThrough(tr)
        tree match {
          case Apply(Apply(cwa, condArgs), args) if isChisel3WhenApply(cwa) => {
            val cond     = CExpLoader(cInfo, condArgs.head)
            val whenBody = bodyFromTree(cInfo, args.head)
            (cond, whenBody)
          }
          case _ => {
            reporter.error(tree.pos, "unknow patten in WhenLoader.whenFromTree")
            (CExp.empty, List.empty)
          }
        }
      }
      def bodyFromTree(cInfo: CircuitInfo, tr: Tree): List[CStatement] = {
        val (tree, _) = passThrough(tr)
        val treeBody = tree match {
          case Block(stats, expr) => stats :+ expr
          case tr                 => List(tr)
        }
        CStatementLoader.fromListTree(cInfo, treeBody)._2
      }

      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, TermName("otherwise")), args) => {
          val (cond, whenBody) = whenFromTree(cInfo, qualifier)
          val otherBody        = bodyFromTree(cInfo, args.head)
          Some((cInfo, Some(When(cond, whenBody, otherBody))))
        }
        case Apply(Apply(cwa, condArgs), args) if isChisel3WhenApply(cwa) => {
          val (cond, whenBody) = whenFromTree(cInfo, tree)
          Some((cInfo, Some(When(cond, whenBody, List.empty))))
        }
        case _ => None
      }
    }
  }

  object AssertLoader extends CStatementObj {
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
