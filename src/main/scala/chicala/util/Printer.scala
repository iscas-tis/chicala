package chicala.util

import scala.tools.nsc.Global

trait Printer extends Format {
  val global: Global
  import global._

  def unprocessedTree(tree: Tree, from: String) = {
    reporter.warning(
      tree.pos,
      s"""not processed in ${from}:
        |tree.tpe:
        |  ${tree.tpe.erasure}
        |tree:
        |  ${tree}
        |tree AST:
        |  ${showFormattedRaw(tree).replace("\n", "\n  ")}
        |source code:""".stripMargin
    )
  }

  def errorTree(tree: Tree, msg: String) = {
    val slicedTraces = stackTraces.drop(1).reduce(_ + "\n  " + _)
    reporter.error(
      tree.pos,
      s"""${msg}:
        |tree.tpe:
        |  ${tree.tpe.erasure}
        |tree AST:
        |  ${showFormattedRaw(tree).replace("\n", "\n  ")}
        |stackTrace:
        |  ${slicedTraces}""".stripMargin
    )
  }

  def stackTraces = {
    Thread
      .currentThread()
      .getStackTrace
      .map(_.toString())
      .toList
      .drop(4)
  }

  def echoTreePos(tree: Tree) = {
    reporter.echo(tree.pos, "here")
  }

  def assertWarning(cond: Boolean, pos: Position, msg: String) = if (!cond) {
    reporter.warning(pos, msg)
  }
  def assertError(cond: Boolean, pos: Position, msg: String) = if (!cond) {
    reporter.error(pos, msg)
  }
}
