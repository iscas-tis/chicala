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

    val connectedSignals = getSignals(tree)
    println(connectedSignals)

    private def getSignals(tree: Tree, hasOtherwise: Boolean = false): ConnectedSignals = {
      tree match {
        // :=
        case Apply(Select(left, TermName("$colon$eq")), rights) => {
          assert(rights.length == 1, "':=' should have only 1 operand on the right")

          val sigs = rights.map(getSignals(_)).reduce(_ ++ _)
          ConnectedSignals(Set(left.toString()), Set(), sigs.dependency)
        }
        // otherwise
        case Apply(Select(when, TermName("otherwise")), body) => {
          // TODO: get otherwise body and fullyConnectedSignals
          getSignals(when, hasOtherwise = true)
        }
        // when
        case Apply(
              Apply(
                Select(Select(Ident(TermName("chisel3")), TermName("when")), TermName("apply")),
                condition
              ),
              body
            ) => {
          assert(condition.length == 1, "`when` should have only 1 condition")

          val tmpls = condition ++ body
          val sigs  = tmpls.map(getSignals(_)).reduce(_ ++ _)

          if (hasOtherwise) sigs
          else ConnectedSignals(Set.empty, sigs.fully ++ sigs.partially, sigs.dependency)
        }
        // TODO: more statement
        case Apply(fun, args) => {
          (fun :: args).map(getSignals(_)).reduce(_ ++ _)
        }
        case Select(qualifier, name) => {
          getSignals(qualifier)
        }
        case _ => ConnectedSignals.empty
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
        case select: Select =>
          select.toString() == "chisel3.internal.sourceinfo.SourceLine.apply"
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
