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

    val (fullyConnectedSignals, partiallyConnectedSignals, dependencySignals) = getSignals(tree)
    println(fullyConnectedSignals)
    println(partiallyConnectedSignals)
    println(dependencySignals)

    private def getSignals(tree: Tree): (Set[String], Set[String], Set[String]) = {
      tree match {
        // :=
        case Apply(Select(left, TermName("$colon$eq")), rights) => {
          assert(rights.length == 1, "':=' should have only 1 operand on the right")

          val deps = rights.map(getSignals(_)._3).flatten.toSet
          (Set(left.toString()), Set(), deps)
        }
        // otherwise
        case Apply(Select(when, TermName("otherwise")), body) => {
          // TODO: get otherwise body and fullyConnectedSignals
          getSignals(when)
        }
        // when
        case Apply(
              Apply(Select(Select(Ident(TermName("chisel3")), TermName("when")), TermName("apply")), condition),
              body
            ) => {
          assert(condition.length == 1, "`when` should have only 1 condition")

          val tmpls = condition ++ body
          tmpls.map(getSignals(_)).reduceLeft((a, b) => (a._1 ++ b._1, a._2 ++ b._2, a._3 ++ b._3))
          // TODO: what if no otherwise?
        }
        // TODO: more statement
        case Apply(fun, args) => {
          val tmpls = fun :: args
          tmpls.map(getSignals(_)).reduceLeft((a, b) => (a._1 ++ b._1, a._2 ++ b._2, a._3 ++ b._3))
        }
        case Select(qualifier, name) => {
          getSignals(qualifier)
        }
        case _ => (Set(), Set(), Set())
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
