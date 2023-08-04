package chicala.convert.frontend

import scala.tools.nsc.Global

trait LiteralsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object LiteralLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case l @ Literal(value) =>
          Some((cInfo, Some(SLiteral(l, EmptyMType))))
        case _ =>
          unprocessedTree(tree, "LiteralLoader")
          None
      }

    }

  }

}
