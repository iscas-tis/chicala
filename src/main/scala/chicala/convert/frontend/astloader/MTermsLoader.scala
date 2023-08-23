package chicala.convert.frontend

import scala.tools.nsc.Global

trait MTermsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object MTermLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: Apply    => ApplyReader(cInfo, tr)
        case _: Block    => BlockReader(cInfo, tr)
        case _: Ident    => IdentReader(cInfo, tr)
        case _: Literal  => LiteralReader(cInfo, tr)
        case _: Select   => SelectReader(cInfo, tr)
        case _: If       => IfReader(cInfo, tr)
        case _: Function => FunctionReader(cInfo, tr)
        case _: Match    => MatchReader(cInfo, tr)
        case _: Assign   => AssignReader(cInfo, tr)
        case EmptyTree   => None
      }
    }

  }

  trait MTermLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])]
  }

}
