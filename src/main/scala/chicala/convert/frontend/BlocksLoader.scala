package chicala.convert.frontend

import scala.tools.nsc.Global

trait BlocksLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object BlockLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SBlock])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Block(stats, expr) =>
          val (newCInfo, cList) = StatementLoader.fromListTree(cInfo, stats :+ expr)
          Some((newCInfo, Some(SBlock(cList, EmptyMType))))
        case _ =>
          unprocessedTree(tree, "BlockLoader")
          None
      }

    }

  }

}
