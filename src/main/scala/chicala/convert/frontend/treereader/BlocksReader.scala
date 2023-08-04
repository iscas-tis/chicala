package chicala.convert.frontend

import scala.tools.nsc.Global

trait BlocksReader { self: Scala2Reader =>
  val global: Global
  import global._

  object BlockReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SBlock])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Block(stats, expr) =>
          val (newCInfo, cList) = StatementReader.fromListTree(cInfo, stats :+ expr)
          Some((newCInfo, Some(SBlock(cList, EmptyMType))))
        case _ =>
          unprocessedTree(tree, "BlockReader")
          None
      }

    }

  }

}
