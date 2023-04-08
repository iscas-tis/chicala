package chicala.sort

import scala.tools.nsc
import nsc.Global

import chicala.util.Format

class StatementProcess[G <: Global]()(implicit val global: G) {
  import global._

  private val fmt = new Format
  import fmt._

  case class ConnectedSignals(val fully: Set[String], val partially: Set[String], val dependency: Set[String]) {
    def ++(that: ConnectedSignals): ConnectedSignals = {
      ConnectedSignals(
        this.fully ++ that.fully,
        this.partially ++ that.partially,
        this.dependency ++ that.dependency
      )
    }
  }
  object ConnectedSignals {
    def empty = ConnectedSignals(Set.empty, Set.empty, Set.empty)
  }

  sealed abstract class Statement(val tree: Tree, val signals: ConnectedSignals)

  case class Connect(
      override val tree: Tree,
      override val signals: ConnectedSignals
  ) extends Statement(tree, signals)
  case class When(
      override val tree: Tree,
      override val signals: ConnectedSignals,
      val condition: Tree,
      val content: Statements
  ) extends Statement(tree, signals)
  case class WhenOtherwise(
      override val tree: Tree,
      override val signals: ConnectedSignals,
      val when: When,
      val content: Statements
  ) extends Statement(tree, signals)

  object Statement {
    def formTree(tree: Tree): Option[Statement] = {
      if (check(tree)) analysis(tree)
      else None
    }

    def analysis(tree: Tree): Option[Statement] = analysis(tree, tree)
    def analysis(tree: Tree, treeRoot: Tree): Option[Statement] = {
      tree match {
        // SourceInfo
        case Apply(fun, args)
            if (fun.tpe.toString().startsWith("(implicit sourceInfo: chisel3.internal.sourceinfo.SourceInfo")) => {
          analysis(fun, treeRoot) // pass through
        }
        case Typed(expr, tpt) => {
          analysis(expr, treeRoot) // pass through
        }
        // otherwise
        case Apply(Select(whenTree, TermName("otherwise")), bodyList) => {
          val when    = analysis(whenTree).get.asInstanceOf[When]
          val content = analysisList(bodyList)

          val whenSigs = when.signals
          val bodySigs = content.signals
          val fully    = whenSigs.fully.intersect(bodySigs.fully) // fully connected in both side
          val tmp      = whenSigs ++ bodySigs

          val signals = ConnectedSignals(fully, tmp.partially ++ tmp.fully -- fully, tmp.dependency)

          Some(WhenOtherwise(treeRoot, signals, when, content))
        }
        // when only
        case Apply(
              Apply(
                Select(Select(Ident(TermName("chisel3")), TermName("when")), TermName("apply")),
                conditionList
              ),
              bodyList
            ) => {
          assert(conditionList.length == 1, "`when` should have only 1 condition")
          val conditionTree = conditionList(0)

          val conditionDeps = processExpression(conditionTree)

          val content  = analysisList(bodyList)
          val bodySigs = content.signals
          val signals  = ConnectedSignals(bodySigs.fully, bodySigs.partially, bodySigs.dependency ++ conditionDeps)

          Some(When(treeRoot, signals, conditionTree, content))
        }
        // :=
        case Apply(Select(left: Select, TermName("$colon$eq")), rights) => {
          assert(rights.length == 1, "':=' should have only 1 operand on the right")

          val deps    = rights.map(processExpression(_)).reduce(_ ++ _)
          val signals = ConnectedSignals(Set(left.toString()), Set(), deps)

          Some(Connect(treeRoot, signals))
        }
        case Apply(fun, args) => {
          println()
          println(tree)
          println()
          println(fun.tpe.toString())
          println("-------------")
          None
        }

        case ValDef(_, _, _, _) => {
          // pass for now
          None
        }
        case _: Tree => {
          global.reporter.error(tree.pos, s"unknown AST:\n${tree.tpe}\n\n${tree}\n\n${showRaw(tree)}**********")
          None
        }
      }

    }
    def analysisList(treeList: List[Tree]): Statements = {
      Statements(
        treeList
          .map({
            case Block(stats, expr) => {
              (stats :+ expr).map(analysis(_))
            }
            case t: Tree => {
              List(analysis(t))
            }
          })
          .flatten
          .flatten
      )
    }

    private def processExpression(tree: Tree): Set[String] = {
      tree match {
        // SourceInfo
        case Apply(fun, args)
            if (fun.tpe.toString().startsWith("(implicit sourceInfo: chisel3.internal.sourceinfo.SourceInfo")) => {
          processExpression(fun) // pass through
        }
        case Typed(expr, tpt) => {
          processExpression(expr) // pass through
        }
        // literal
        case Select(
              Apply(Select(Select(Ident(TermName("chisel3")), TermName("package")), TermName("fromIntToLiteral")), _),
              _
            ) => {
          Set.empty
        }
        // signal
        case s: Select if (List("chisel3.Bool", "chisel3.UInt").contains(tree.tpe.toString())) => {
          Set(s.toString())
        }
        // operator
        case Apply(Select(left: Select, op), rights) if (List("do_$plus").contains(op.toString())) => {
          assert(rights.length == 1, "'do_+' should have only 1 operand on the right")

          val sigs = rights.map(processExpression(_)).reduce(_ ++ _)
          Set(left.toString()) ++ sigs
        }
        case _: Tree => {
          println()
          println(tree.tpe)
          println(tree)
          println()
          println(showRaw(tree))
          println("processExpression")
          println("**********")
          Set.empty
        }
      }
    }

    def filter(tree: Tree): Option[Tree] = {
      if (check(tree)) Some(tree)
      else None
    }
    def check(tree: Tree): Boolean = {
      tree.exists {
        case Apply(fun, args) =>
          fun.tpe.toString().startsWith("(implicit sourceInfo: chisel3.internal.sourceinfo.SourceInfo")
        case _ => false
      }
    }
  }

  /** Collection of statement
    *
    * @param body
    *   list of statement
    */
  case class Statements(val body: List[Statement], val signals: ConnectedSignals)
  object Statements {
    def apply(body: List[Statement]): Statements = Statements(body, body.map(_.signals).reduce(_ ++ _))

    def fromTreeList(treeList: List[Tree]): Statements = {
      Statements(treeList.map(Statement.formTree(_)).flatten)
    }
  }
}
