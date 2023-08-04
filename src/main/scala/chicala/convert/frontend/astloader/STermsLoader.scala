package chicala.convert.frontend

import scala.tools.nsc.Global

trait STermsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object SApplyLoader extends MTermLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SApply])] = {
      val tree = passThrough(tr)._1
      tree match {
        case Apply(fun, args) =>
          passThrough(fun)._1 match {
            case s: Select =>
              Some(
                (cInfo, Some(SApply(SSelect(s, EmptyMType), args.map(MTermLoader(cInfo, _).get._2.get), EmptyMType)))
              )
            case _ => None
          }
        case _ => None
      }
    }
  }
}
