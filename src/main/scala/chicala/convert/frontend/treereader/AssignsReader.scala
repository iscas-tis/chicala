package chicala.convert.frontend

import scala.tools.nsc.Global

trait AssignsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object AssignReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      firstMatchIn[MTerm](
        cInfo,
        tr,
        List(
          ConnectLoader(_, _)
        )
      ) match {
        case Some(value) => Some(value)
        case None =>
          unprocessedTree(tr, "AssignReader")
          None
      }
    }
  }
}
