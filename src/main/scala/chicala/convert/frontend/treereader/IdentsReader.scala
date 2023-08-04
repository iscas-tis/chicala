package chicala.convert.frontend

import scala.tools.nsc.Global

trait IdentsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object IdentReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case i @ Ident(name) =>
          if (isChiselType(i))
            Some((cInfo, Some(SignalRef(i, cInfo.getCType(i)))))
          else {
            unprocessedTree(tree, "IdentReader")
            None
          }
        case _ =>
          unprocessedTree(tree, "IdentReader")
          None
      }

    }

  }

}
