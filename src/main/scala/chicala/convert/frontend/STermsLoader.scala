package chicala.convert.frontend

import scala.tools.nsc.Global

trait STermsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object SApplyLoader extends MTermLoaderObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SApply])] = {
      val tree = passThrough(tr)._1
      tree match {
        case Apply(fun, args) =>
          passThrough(fun)._1 match {
            case s: Select =>
              Some((cInfo, Some(SApply(SSelect(s, EmptyMType), args.map(CExpLoader(cInfo, _)), EmptyMType))))
            case _ => None
          }
        case _ => None
      }
    }
  }
  object SBlockLoader extends MTermLoaderObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SBlock])] = {
      val tree = passThrough(tr)._1
      tree match {
        case Block(stats, expr) =>
          val (newCInfo, cList) = MStatementLoader.fromListTree(cInfo, stats :+ expr)
          Some((newCInfo, Some(SBlock(cList, EmptyMType))))
        case _ =>
          Some((cInfo, Some(SBlock(List(CExpLoader(cInfo, tr)), EmptyMType))))
      }
    }
  }

}
