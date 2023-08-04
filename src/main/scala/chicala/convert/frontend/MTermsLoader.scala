package chicala.convert.frontend

import scala.tools.nsc.Global

trait MTermsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  trait MTermLoaderObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])]
  }

  object MTermLoader {
    private def someMTermIn(
        cInfo: CircuitInfo,
        tree: Tree,
        objs: List[MTermLoaderObj]
    ): Option[(CircuitInfo, Option[MTerm])] = {
      val ls = objs.map(_.apply(cInfo, tree)).flatten

      assert(ls.length <= 1, "should be at most one term")
      ls match {
        case head :: next => Some(head)
        case Nil => {
          unprocessedTree(tree, "MTermLoader.someMTermIn")
          None
        }
      }
    }

    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, someTpe) = passThrough(tr)
      tree match {
        case a: Apply => {
          someMTermIn(cInfo, tr, List(AssertLoader, WhenLoader, ConnectLoader)) match {
            case Some(value) => Some(value)
            case None        => SApplyLoader(cInfo, tr)
          }
        }
        // empty statement
        case Literal(Constant(())) =>
          None
        case _ => {
          unprocessedTree(tree, "MTermLoader case _")
          None
        }
      }
    }

  }

  object ConnectLoader extends MTermLoaderObj {
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
  object WhenLoader extends MTermLoaderObj {
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

  object AssertLoader extends MTermLoaderObj {
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

  object CExpLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): MTerm = {
      val (tree, tpt) = passThrough(tr)

      tree match {
        case Apply(Select(qualifier, name), args) if isChiselType(qualifier) =>
          val opName = name.toString()
          COp(opName) match {
            case Some(op) =>
              val signalInfo = CTypeLoader(tpt)
              CApply(op, signalInfo, (qualifier :: args).map(CExpLoader(cInfo, _)))
            case None => {
              unprocessedTree(tree, "CExpLoader")
              EmptyMTerm // empty
            }
          }
        case Apply(Select(qualifier, name), args) if isChiselLiteralType(qualifier) => {
          // 0.U(1.W)
          val litTree = qualifier.asInstanceOf[Apply].args.head
          val litExp  = CExpLoader(cInfo, litTree).asInstanceOf[STerm]
          val width   = CTypeLoader.getSomeWidth(args).get

          name.toString() match {
            case "U" => Lit(litExp, UInt(Node, Undirect)) // width
            case "S" => Lit(litExp, SInt(Node, Undirect)) // width
            case _ =>
              reporter.error(tree.pos, s"Unknow name in CExp ${name}")
              EmptyMTerm
          }
        }
        case a @ Apply(fun, args) =>
          val f     = passThrough(fun)._1
          val fName = f.toString()
          COp(fName) match {
            case Some(op) =>
              val signalInfo = CTypeLoader.fromTypeTree(tpt)
              CApply(op, signalInfo, args.map(CExpLoader(cInfo, _)))
            case None =>
              SApplyLoader(cInfo, a).get._2.get // SApply has no SignalInfo
          }
        case s @ Select(qualifier, name) =>
          if (isChiselType(tpt)) {
            if (isChiselType(qualifier)) {
              COp(name.toString()) match {
                case Some(op) =>
                  CApply(op, CTypeLoader(tpt), List(CExpLoader(cInfo, qualifier)))
                case None =>
                  if (isChiselType(s))
                    SignalRef(s, cInfo.getCType(s))
                  else {
                    reporter.error(tree.pos, s"Unknow op name in CExp ${name}")
                    EmptyMTerm
                  }
              }
            } else if (isChiselLiteralType(qualifier)) {
              val litTree = qualifier.asInstanceOf[Apply].args.head
              val litExp  = CExpLoader(cInfo, litTree).asInstanceOf[STerm]

              name.toString() match {
                case "U" => Lit(litExp, UInt(Node, Undirect))
                case "S" => Lit(litExp, SInt(Node, Undirect))
                case _ =>
                  reporter.error(tree.pos, s"Unknow name in CExp ${name}")
                  EmptyMTerm
              }
            } else {
              SignalRef(s, cInfo.getCType(s))
            }
          } else {
            SSelect(s, EmptyMType) // SSelect has no SignalInfo
          }
        case i @ Ident(name) =>
          if (isChiselType(i))
            SignalRef(i, cInfo.getCType(i))
          else {
            unprocessedTree(tree, "CExpLoader.case.Ident")
            EmptyMTerm
          }
        case l @ Literal(value) =>
          SLiteral(l, EmptyMType) // SLiteral has no SignalInfo

        case EmptyTree => EmptyMTerm
        case _ => {
          unprocessedTree(tree, "CExpLoader")
          EmptyMTerm
        }

      }
    }
  }

}
