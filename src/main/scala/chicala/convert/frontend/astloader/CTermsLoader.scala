package chicala.convert.frontend

import scala.tools.nsc.Global

trait CTermsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object ConnectLoader extends MTermLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Connect])] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Select(termName: Select, TermName("$colon$eq")), args) if isChiselType(termName) =>
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
  object CApplyLoader extends MTermLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[CApply])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, name), args) if isChiselType(qualifier) =>
          val opName = name.toString()
          COpLoader(opName) match {
            case Some(op) =>
              val tpe = CTypeLoader.fromTpt(tpt).get.setInferredWidth
              Some((cInfo, Some(CApply(op, tpe, (qualifier :: args).map(MTermLoader(cInfo, _).get._2.get)))))
            case None =>
              unprocessedTree(tr, s"CApplyLoader `${opName}`")
              None
          }
        case a @ Apply(fun, args) => {
          val f     = passThrough(fun)._1
          val fName = f.toString()
          COpLoader(fName) match {
            case Some(op) =>
              val tpe = CTypeLoader.fromTpt(tpt).get.setInferredWidth
              Some((cInfo, Some(CApply(op, tpe, args.map(MTermLoader(cInfo, _).get._2.get)))))
            case None => None
          }
        }
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

  object SwitchLoader extends MTermLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Switch])] = {
      val (tree, tpt) = passThrough(tr)
      if (!isChisel3UtilSwitchContextType(tpt)) return None

      tree match {
        case Apply(Apply(Select(qualifier, TermName("is")), vArgs), bodyArgs) =>
          val switch = SwitchLoader(cInfo, qualifier).get._2.get

          val v = MTermLoader(cInfo, vArgs.head).get._2.get
          val body = MTermLoader(cInfo, bodyArgs.head).get._2.get match {
            case SBlock(body, _) => body
            case x               => List(x)
          }
          Some((cInfo, Some(switch.appended(v, body))))
        case Apply(Select(New(t), termNames.CONSTRUCTOR), args) if isChisel3UtilSwitchContextType(t) =>
          val cond = MTermLoader(cInfo, args.head).get._2.get
          Some((cInfo, Some(Switch(cond, List.empty))))
        case _ =>
          errorTree(tr, "Unknow structure in SwitchLoader")
          None
      }

    }
  }

  object AssertLoader extends MTermLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Assert])] = {
      val (tree, _) = passThrough(tr)
      if (isReturnAssert(tree)) {
        tree match {
          case Apply(Ident(TermName("_applyWithSourceLinePrintable")), args) =>
            Some(cInfo, Some(Assert(MTermLoader(cInfo, args.head).get._2.get)))
          case _ => None
        }
      } else None
    }
  }

  object LitLoader extends MTermLoader {
    private def nameToSomeLitGen(name: Name): (STerm, CSize) => Option[Lit] = {
      name.toString() match {
        case "U" => (litExp, width) => Some(Lit(litExp, UInt(width, Node, Undirect)))
        case "S" => (litExp, width) => Some(Lit(litExp, SInt(width, Node, Undirect)))
        case "B" => (litExp, width) => Some(Lit(litExp, Bool(Node, Undirect)))
        case _   => (litExp, width) => None
      }
    }
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Lit])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, name), args) if isChiselLiteralType(qualifier) => {
          // 0.U(1.W)
          val litTree = qualifier.asInstanceOf[Apply].args.head
          val litExp  = MTermLoader(cInfo, litTree).get._2.get.asInstanceOf[STerm]
          val width = CTypeLoader.getWidth(cInfo, args) match {
            case k: KnownSize => k
            case _            => InferredSize
          }
          nameToSomeLitGen(name)(litExp, width) match {
            case Some(lit) => Some((cInfo, Some(lit)))
            case None =>
              errorTree(tree, "Unknow name in CExp")
              None
          }

        }
        case Select(qualifier, name) if isChiselLiteralType(qualifier) => {
          // someInt.U without width
          val litTree = qualifier.asInstanceOf[Apply].args.head
          val litExp  = MTermLoader(cInfo, litTree).get._2.get.asInstanceOf[STerm]

          nameToSomeLitGen(name)(litExp, InferredSize) match {
            case Some(lit) => Some((cInfo, Some(lit)))
            case None =>
              errorTree(tree, "Unknow name in CExp")
              None
          }
        }
        case _ => None
      }
    }
  }
}
