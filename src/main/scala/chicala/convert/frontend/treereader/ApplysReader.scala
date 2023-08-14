package chicala.convert.frontend

import scala.tools.nsc.Global

trait ApplysReader { self: Scala2Reader =>
  val global: Global
  import global._

  object ApplyReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      firstMatchIn[MTerm](
        cInfo,
        tr,
        List(
          AssertLoader(_, _),
          WhenLoader(_, _),
          SwitchLoader(_, _),
          ConnectLoader(_, _),
          CApplyLoader(_, _),
          LitLoader(_, _),
          STupleLoader(_, _),
          SApplyLoader(_, _)
        )
      ) match {
        case Some(value) => Some(value)
        case None =>
          unprocessedTree(tr, "ApplyReader")
          None
      }
    }
  }
}
