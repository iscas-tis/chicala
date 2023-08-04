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

  def firstMatchIn[T <: MStatement](
      cInfo: CircuitInfo,
      tree: Tree,
      objs: List[(CircuitInfo, Tree) => Option[(CircuitInfo, Option[T])]]
  ): Option[(CircuitInfo, Option[T])] = {
    objs.foldLeft(None: Option[(CircuitInfo, Option[T])]) { case (past, obj) =>
      past match {
        case Some(_) => past
        case None    => obj(cInfo, tree)
      }
    }
  }

}
