package chicala.sort

import scala.tools.nsc
import nsc.Global

import chicala.util.Format

class TopologicalSort[G <: Global]()(implicit val global: G) {
  import global._

  private val fmt = new Format
  import fmt._

  /** Statement with id
    *
    * @param id
    *   identify original order
    * @param tree
    *   AST statement
    */
  case class Statement(val id: List[Int], val tree: Tree) {

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

    val connectedSignals = getConnectedSignals(tree)
    println(connectedSignals)

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

    private def getConnectedSignals(tree: Tree, hasOtherwise: Boolean = false): ConnectedSignals = {
      tree match {
        // SourceInfo
        case Apply(fun, args)
            if (fun.tpe.toString().startsWith("(implicit sourceInfo: chisel3.internal.sourceinfo.SourceInfo")) => {
          getConnectedSignals(fun, hasOtherwise) // pass through
        }
        case Typed(expr, tpt) => {
          getConnectedSignals(expr, hasOtherwise) // pass through
        }
        case Block(stats, expr) => {
          (stats :+ expr).map(getConnectedSignals(_)).reduce(_ ++ _)
        }
        // otherwise
        case Apply(Select(when, TermName("otherwise")), body) => {
          val whenSigs  = getConnectedSignals(when, hasOtherwise = true)
          val otherSigs = body.map(getConnectedSignals(_)).reduce(_ ++ _)

          val fully = whenSigs.fully.intersect(otherSigs.fully)

          val tmp = whenSigs ++ otherSigs
          ConnectedSignals(fully, tmp.partially ++ tmp.fully -- fully, tmp.dependency)
        }
        // when only
        case Apply(
              Apply(
                Select(Select(Ident(TermName("chisel3")), TermName("when")), TermName("apply")),
                condition
              ),
              body
            ) => {
          assert(condition.length == 1, "`when` should have only 1 condition")

          val conditionDeps                                  = processExpression(condition(0))
          val ConnectedSignals(fully, partially, dependency) = body.map(getConnectedSignals(_)).reduce(_ ++ _)

          if (hasOtherwise)
            ConnectedSignals(fully, partially, conditionDeps ++ dependency)
          else
            ConnectedSignals(Set.empty, fully ++ partially, dependency ++ conditionDeps)
        }
        // :=
        case Apply(Select(left: Select, TermName("$colon$eq")), rights) => {
          assert(rights.length == 1, "':=' should have only 1 operand on the right")

          val deps = rights.map(processExpression(_)).reduce(_ ++ _)
          ConnectedSignals(Set(left.toString()), Set(), deps)
        }
        case Apply(fun, args) => {
          println()
          println(tree)
          println()
          println(fun.tpe.toString())
          println("-------------")

          ConnectedSignals.empty
        }

        case ValDef(_, _, _, _) => {
          // pass for now
          ConnectedSignals.empty
        }
        case _: Tree => {
          global.reporter.error(tree.pos, s"unknown AST:\n${tree.tpe}\n\n${tree}\n\n${showRaw(tree)}**********")
          ConnectedSignals.empty
        }
      }
    }

    def expand: List[Statement] = {
      // TODO: add expand code
      List(this)
    }
    def expandAll: List[Statement] = {
      val expandOnes = expand
      if (expandOnes.length == 1 && expandOnes(0) == this) {
        List(this)
      } else {
        expandOnes.map(_.expandAll).flatten
      }
    }
  }
  object Statement {
    def formTree(tree: Tree): Option[Statement] = {
      if (check(tree)) Some(Statement(List.empty, tree))
      else None
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
  case class Statements(val body: List[Statement]) {
    def expand: Statements = {
      new Statements(
        body.map(_.expand).flatten
      )
    }
    def expandAll: Statements = {
      new Statements(
        body.map(_.expandAll).flatten
      )
    }
  }
  object Statements {
    def fromTreeList(treeList: List[Tree]): Statements = {
      val flitered = treeList.map(Statement.filter(_)).flatten
      new Statements(
        (0 until flitered.length)
          .zip(flitered)
          .map { case (id, tree) => Statement(List(id), tree) }
          .toList
      )
    }
  }
}
