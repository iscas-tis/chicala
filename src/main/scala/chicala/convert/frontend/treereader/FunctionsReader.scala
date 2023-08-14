package chicala.convert.frontend

import scala.tools.nsc.Global

trait FunctionsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object FunctionReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SFunction])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Function(vparams, body) =>
          val (newCInfo, vps) = vparams.foldLeft((cInfo, List.empty[MValDef])) { case ((cf, ls), t) =>
            ValDefReader(cf, t) match {
              case Some((ncf, Some(mvd: MValDef))) => (ncf, ls :+ mvd)
              case x =>
                unprocessedTree(t, "FunctionReader vparams")
                (cf, ls)
            }
          }

          val b = MTermLoader(newCInfo, body).get._2.get
          Some((cInfo, Some(SFunction(vps, b))))

        case _ =>
          unprocessedTree(tree, "FunctionReader")
          None
      }
    }

  }

}
