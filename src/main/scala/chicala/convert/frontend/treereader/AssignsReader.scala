package chicala.convert.frontend

import scala.tools.nsc.Global

trait AssignsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object AssignReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case Assign(lhs, rhs) => {
          val left  = MTermLoader(cInfo, lhs).get._2.get
          val right = MTermLoader(cInfo, rhs).get._2.get
          Some(cInfo, Some(SAssign(left, right)))
        }
        case _ => None
      }
    }
  }
}
