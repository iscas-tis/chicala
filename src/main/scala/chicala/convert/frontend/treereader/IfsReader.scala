package chicala.convert.frontend

import scala.tools.nsc.Global

trait IfsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object IfReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SIf])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case i @ If(cond, thenp, elsep) => {
          val c   = STermLoader(cInfo, cond).get._2.get
          val t   = MTermLoader(cInfo, thenp).get._2.get
          val e   = MTermLoader(cInfo, elsep).get._2.get
          val tpe = MTypeLoader(tpt)
          Some((cInfo, Some(SIf(c, t, e, tpe))))
        }
        case _ =>
          unprocessedTree(tree, "IfsReader")
          None
      }
    }
  }
}
