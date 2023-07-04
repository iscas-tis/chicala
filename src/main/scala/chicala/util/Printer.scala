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
        |  ${tree.tpe}
        |tree:
        |  ${tree}
        |tree AST:
        |  ${showFormattedRaw(tree, 3).replace("\n", "\n  ")}
        |source code:""".stripMargin
    )
  }
}
