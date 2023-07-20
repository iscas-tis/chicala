package chicala.ast

import scala.tools.nsc.Global
import chicala.util.Format

trait CStatements { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CStatement {
    val relatedSignals: RelatedSignals
  }
  trait CStatementObj {
    def fromTree(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[CStatement])]
  }

  object CStatement extends CStatementObj {
    private def someStatementIn(
        cInfo: CircuitInfo,
        tree: Tree,
        objs: List[CStatementObj]
    ): Option[(CircuitInfo, Option[CStatement])] = {
      val ls = objs.map(_.fromTree(cInfo, tree)).flatten

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
        fromTree(info, tr) match {
          case Some((newInfo, Some(newStat))) => (newInfo, newStat :: past)
          case Some((newInfo, None))          => (newInfo, past)
          case None                           => (info, past)
        }
      }) match { case (info, past) => (info, past.reverse) }
    }

    def fromTree(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[CStatement])] = {
      val (tree, someTpe) = passThrough(tr)
      tree match {
        case ValDef(mods, name, tpt, rhs) => {
          if (mods.isParamAccessor) None // pass ParamAccessor
          else {
            someStatementIn(cInfo, tree, List(IoDef, WireDef))
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
              unprocessedTree(tree, "CStatement.fromTree case DefDef")
              None
            }
          }
        }
        case _: Apply => {
          someStatementIn(cInfo, tree, List(Assert, When, Connect))
        }
        // empty statement
        case Literal(Constant(())) =>
          None
        case _ => {
          unprocessedTree(tree, "CStatement.fromTree case _")
          None
        }
      }
    }

  }

  sealed abstract class SignalDef extends CStatement

  case class IoDef(name: TermName, info: SignalInfo, circuitName: TypeName) extends SignalDef {
    val relatedSignals: RelatedSignals = RelatedSignals(
      info.dataType match {
        case b: Bundle => b.subSignals.map(s => s"${name.toString()}.${s}")
        case _         => Set(name.toString())
      },
      Set.empty,
      Set.empty
    )
  }
  object IoDef extends CStatementObj {
    def fromTree(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[IoDef])] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case ValDef(mods, nameTmp, tpt, Apply(TypeApply(Select(_, TermName("IO")), _), args)) => {
          val someBundleDef = args.head match {
            case Block(stats, expr) => BundleDef.fromTree(stats.head)
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

  case class WireDef(name: TermName, info: SignalInfo, circuitName: TypeName) extends SignalDef {
    // not empty when WireDef with init val
    val relatedSignals: RelatedSignals = RelatedSignals(Set(name.toString()), Set.empty, Set.empty)
  }
  object WireDef extends CStatementObj {
    def fromTree(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[WireDef])] = {
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
              val dataType   = CDataType.fromTree(args.head)
              val signalInfo = SignalInfo(Wire, dataType)
              val newInfo    = cInfo.updatedSignal(name, signalInfo)
              Some(newInfo, Some(WireDef(name, signalInfo, cInfo.name)))
            case _ => None
          }
        case _ => None
      }

    }
  }

  case class RegDef() extends SignalDef {
    // not empty when RegDef with init val
    val relatedSignals: RelatedSignals = RelatedSignals.empty
  }
  case class NodeDef() extends SignalDef {
    // for now
    val relatedSignals: RelatedSignals = RelatedSignals.empty
  }

  case class Connect(left: SignalRef, expr: CExp) extends CStatement {
    val relatedSignals: RelatedSignals =
      RelatedSignals(Set(left.name.toString()), Set.empty, expr.signals)
  }
  object Connect extends CStatementObj {
    def fromTree(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Connect])] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Select(termName: Select, TermName("$colon$eq")), args) =>
          assert(args.length == 1, "should have one right expr")
          val left  = CExp.fromTree(cInfo, termName)
          val right = CExp.fromTree(cInfo, args.head)
          left match {
            case x: SignalRef => Some(cInfo, Some(Connect(x, right)))
            case _ =>
              unprocessedTree(termName, "Connect.fromTree")
              None
          }
        case _ => None
      }
    }
  }
  case class BulkConnect() extends CStatement {
    // for now
    val relatedSignals: RelatedSignals = RelatedSignals.empty
  }

  case class When(cond: CExp, whenBody: List[CStatement], otherBody: List[CStatement]) extends CStatement {
    val relatedSignals: RelatedSignals = {
      val whenRS     = whenBody.map(_.relatedSignals).fold(RelatedSignals.empty)(_ ++ _)
      val otherRS    = otherBody.map(_.relatedSignals).fold(RelatedSignals.empty)(_ ++ _)
      val fully      = whenRS.fully.intersect(otherRS.fully)
      val partially  = whenRS.partially ++ otherRS.partially ++ (whenRS.fully ++ otherRS.fully -- fully)
      val dependency = whenRS.dependency ++ otherRS.dependency ++ cond.signals
      RelatedSignals(fully, partially, dependency)
    }
  }
  object When extends CStatementObj {
    def fromTree(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[When])] = {
      def whenFromTree(cInfo: CircuitInfo, tr: Tree): (CExp, List[CStatement]) = {
        val (tree, _) = passThrough(tr)
        tree match {
          case Apply(Apply(cwa, condArgs), args) if Chisel3WhenApply(cwa) => {
            val cond     = CExp.fromTree(cInfo, condArgs.head)
            val whenBody = bodyFromTree(cInfo, args.head)
            (cond, whenBody)
          }
          case _ => {
            reporter.error(tree.pos, "unknow patten in When.fromTree.whenFromTree")
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
        CStatement.fromListTree(cInfo, treeBody)._2
      }

      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, TermName("otherwise")), args) => {
          val (cond, whenBody) = whenFromTree(cInfo, qualifier)
          val otherBody        = bodyFromTree(cInfo, args.head)
          Some((cInfo, Some(When(cond, whenBody, otherBody))))
        }
        case Apply(Apply(cwa, condArgs), args) if Chisel3WhenApply(cwa) => {
          val (cond, whenBody) = whenFromTree(cInfo, tree)
          Some((cInfo, Some(When(cond, whenBody, List.empty))))
        }
        case _ => None
      }
    }
  }

  case class Assert(exp: CExp) extends CStatement {
    val relatedSignals: RelatedSignals = RelatedSignals(Set.empty, Set.empty, exp.signals)
  }
  object Assert extends CStatementObj {
    def fromTree(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Assert])] = {
      val (tree, _) = passThrough(tr)
      if (ReturnAssert(tree)) {
        tree match {
          case Apply(Ident(TermName("_applyWithSourceLinePrintable")), args) =>
            Some(cInfo, Some(Assert(CExp.fromTree(cInfo, args.head))))
          case _ => None
        }
      } else None
    }
  }

}
