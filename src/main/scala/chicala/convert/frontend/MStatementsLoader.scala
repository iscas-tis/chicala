package chicala.convert.frontend

import scala.tools.nsc.Global

trait MStatementsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object MDefLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MDef])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: ValDef => ValDefReader(cInfo, tr)
        case _: DefDef => DefDefReader(cInfo, tr)
      }
    }
  }

  object MTermLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: Apply => ApplyReader(cInfo, tr)
      }
    }
  }
}
