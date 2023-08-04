package chicala.convert.frontend

import scala.tools.nsc.Global

trait ApplysReader { self: Scala2Reader =>
  val global: Global
  import global._

  object ApplyReader {

    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, name), args) if isChiselType(qualifier) =>
          val opName = name.toString()
          COp(opName) match {
            case Some(op) =>
              val signalInfo = CTypeLoader(tpt)
              Some((cInfo, Some(CApply(op, signalInfo, (qualifier :: args).map(MTermLoader(cInfo, _).get._2.get)))))
            case None =>
              unprocessedTree(tr, "ApplyReader")
              None
          }
        case Apply(Select(qualifier, name), args) if isChiselLiteralType(qualifier) => {
          // 0.U(1.W)
          val litTree = qualifier.asInstanceOf[Apply].args.head
          val litExp  = MTermLoader(cInfo, litTree).asInstanceOf[STerm]
          val width   = CTypeLoader.getSomeWidth(args).get

          name.toString() match {
            case "U" => Some((cInfo, Some(Lit(litExp, UInt(Node, Undirect))))) // width
            case "S" => Some((cInfo, Some(Lit(litExp, SInt(Node, Undirect))))) // width
            case _ =>
              reporter.error(tree.pos, s"Unknow name in CExp ${name}")
              None
          }
        }
        case a @ Apply(fun, args) => {
          val f     = passThrough(fun)._1
          val fName = f.toString()
          COp(fName) match {
            case Some(op) =>
              val signalInfo = CTypeLoader.fromTypeTree(tpt)
              Some((cInfo, Some(CApply(op, signalInfo, args.map(MTermLoader(cInfo, _).get._2.get)))))
            case None => {
              someMTermIn(cInfo, tr, List(AssertLoader, WhenLoader, ConnectLoader)) match {
                case Some(value) => Some(value)
                case None        => SApplyReader(cInfo, tr)
              }
            }
          }

        }
        case _ =>
          unprocessedTree(tree, "ApplyReader")
          None
      }

    }

    trait MTermLoader {
      def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])]
    }
    private def someMTermIn(
        cInfo: CircuitInfo,
        tree: Tree,
        objs: List[MTermLoader]
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

    object ConnectLoader extends MTermLoader {
      def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Connect])] = {
        val (tree, _) = passThrough(tr)
        tree match {
          case Apply(Select(termName: Select, TermName("$colon$eq")), args) =>
            assert(args.length == 1, "should have one right expr")
            val left  = MTermLoader(cInfo, termName).get._2.get
            val right = MTermLoader(cInfo, args.head).get._2.get
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
    object WhenLoader extends MTermLoader {
      def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[When])] = {
        def bodyFromTree(cInfo: CircuitInfo, tr: Tree): List[MStatement] = {
          val (tree, _) = passThrough(tr)
          val treeBody = tree match {
            case Block(stats, expr) => stats :+ expr
            case tr                 => List(tr)
          }
          StatementReader.fromListTree(cInfo, treeBody)._2
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
            val cond     = MTermLoader(cInfo, condArgs.head).get._2.get
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

            val elseCond     = MTermLoader(cInfo, condArgs.head).get._2.get
            val elseWhen     = When(elseCond, bodyFromTree(cInfo, args.head), List.empty)
            val whenElseWhen = pushBackElseWhen(when, elseWhen)

            Some((cInfo, Some(whenElseWhen)))
          }
          case _ => None
        }
      }
    }

    object AssertLoader extends MTermLoader {
      def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Assert])] = {
        val (tree, _) = passThrough(tr)
        if (isReturnAssert(tree)) {
          tree match {
            case Apply(Ident(TermName("_applyWithSourceLinePrintable")), args) =>
              Some(cInfo, Some(Assert(StatementReader(cInfo, args.head).get._2.get.asInstanceOf[MTerm])))
            case _ => None
          }
        } else None
      }
    }

    object SApplyReader extends MTermLoader {
      def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SApply])] = {
        val tree = passThrough(tr)._1
        tree match {
          case Apply(fun, args) =>
            passThrough(fun)._1 match {
              case s: Select =>
                Some(
                  (cInfo, Some(SApply(SSelect(s, EmptyMType), args.map(MTermLoader(cInfo, _).get._2.get), EmptyMType)))
                )
              case _ => None
            }
          case _ => None
        }
      }
    }
  }

}
