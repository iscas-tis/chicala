package chicala.convert.frontend

import scala.tools.nsc.Global

trait StatementsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object StatementLoader {
    def fromListTree(cInfo: CircuitInfo, body: List[Tree]): (CircuitInfo, List[MStatement]) = {
      (body.foldLeft((cInfo, List.empty[MStatement])) { case ((info, past), tr) =>
        StatementLoader(info, tr) match {
          case Some((newInfo, Some(newStat))) => (newInfo, newStat :: past)
          case Some((newInfo, None))          => (newInfo, past)
          case None                           => (info, past)
        }
      }) match { case (info, past) => (info, past.reverse) }
    }

    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MStatement])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: ValDef | _: DefDef => MDefLoader(cInfo, tr)
        case _                     => MTermLoader(cInfo, tr)
      }
    }
  }
  object MDefLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MDef])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: ValDef => ValDefLoader(cInfo, tr)
        case _: DefDef => DefDefLoader(cInfo, tr)
      }
    }
  }

  object MTermLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: Apply => ApplyLoader(cInfo, tr)
      }
    }
  }
}
