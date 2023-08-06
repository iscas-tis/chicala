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
        |  ${showFormattedRaw(tree, 3).replace("\n", "\n  ")}
        |source code:""".stripMargin
    )
  }

  def errorTree(tree: Tree, msg: String) = {
    val stackTraces = Thread
      .currentThread()
      .getStackTrace
      .map(_.toString())
    val slicedTraces = stackTraces
      .slice(1, 11.min(stackTraces.length))
      .reduce(_ + "\n  " + _)
    reporter.error(
      tree.pos,
      s"""${msg}:
        |tree.tpe:
        |  ${tree.tpe.erasure}
        |tree AST:
        |  ${showFormattedRaw(tree, 3).replace("\n", "\n  ")}
        |stackTrace:
        |  ${slicedTraces}
        |source code:""".stripMargin
    )
  }
}
