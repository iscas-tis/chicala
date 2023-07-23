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
        case ValDef(mods, name, tpt, rhs) => {
          if (mods.isParamAccessor) None // pass ParamAccessor
          else {
            someStatementIn(cInfo, tree, List(IoDefLoader, WireDefLoader))
          }
        }
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
          name match {
            // constructor of this class
            case termNames.CONSTRUCTOR => {
              // register construct param
              val params = vparamss.flatten.map(x => (x.name, x.tpt.asInstanceOf[TypeTree]))
              Some((cInfo.updatedParams(params), None))
            }
            // accessor of signal
            case x: TermName if cInfo.signal.contains(x) => None
            case _ => {
              unprocessedTree(tree, "CStatementLoader case DefDef")
              None
            }
          }
        }
        case _: Apply => {
          someStatementIn(cInfo, tree, List(AssertLoader, WhenLoader, ConnectLoader))
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

  object IoDefLoader extends CStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[IoDef])] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case ValDef(mods, nameTmp, tpt, Apply(TypeApply(Select(_, TermName("IO")), _), args)) => {
          val someBundleDef = args.head match {
            case Block(stats, expr) => BundleDefLoader(stats.head)
            case _                  => None
          }
          val name = nameTmp.stripSuffix(" ")

          val bundle  = someBundleDef.get.bundle
          val newInfo = cInfo.updatedSignal(name, SignalInfo(Io, bundle))
          val ioDef   = IoDef(name, SignalInfo(Io, bundle), cInfo.name)

          Some((newInfo, Some(ioDef)))
        }
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
              Some(newInfo, Some(WireDef(name, signalInfo, cInfo.name)))
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
