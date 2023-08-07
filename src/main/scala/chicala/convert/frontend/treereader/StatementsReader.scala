package chicala.convert.frontend

import scala.tools.nsc.Global

trait StatementsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object StatementReader {
    def fromListTree(cInfo: CircuitInfo, body: List[Tree]): (CircuitInfo, List[MStatement]) = {
      (body.foldLeft((cInfo, List.empty[MStatement])) { case ((info, past), tr) =>
        StatementReader(info, tr) match {
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

}