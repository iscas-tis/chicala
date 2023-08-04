package chicala.convert.frontend

import scala.tools.nsc.Global

trait IdentsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object IdentLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case i @ Ident(name) =>
          if (isChiselType(i))
            Some((cInfo, Some(SignalRef(i, cInfo.getCType(i)))))
          else {
            unprocessedTree(tree, "CExpLoader.case.Ident")
            None
          }
        case _ =>
          unprocessedTree(tree, "BlockLoader")
          None
      }

    }

  }

}
