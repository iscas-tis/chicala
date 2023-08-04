package chicala.convert.frontend

import scala.tools.nsc.Global

trait SelectsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object SelectLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case s @ Select(qualifier, name) =>
          if (isChiselType(tpt)) {
            if (isChiselType(qualifier)) {
              COp(name.toString()) match {
                case Some(op) =>
                  Some((cInfo, Some(CApply(op, CTypeLoader(tpt), List(MTermLoader(cInfo, qualifier).get._2.get)))))
                case None =>
                  if (isChiselType(s))
                    Some((cInfo, Some(SignalRef(s, cInfo.getCType(s)))))
                  else {
                    reporter.error(tree.pos, s"Unknow op name in CExp ${name}")
                    Some((cInfo, None))
                  }
              }
            } else if (isChiselLiteralType(qualifier)) {
              val litTree = qualifier.asInstanceOf[Apply].args.head
              val litExp  = MTermLoader(cInfo, litTree).asInstanceOf[STerm]

              name.toString() match {
                case "U" => Some((cInfo, Some(Lit(litExp, UInt(Node, Undirect)))))
                case "S" => Some((cInfo, Some(Lit(litExp, SInt(Node, Undirect)))))
                case _ =>
                  reporter.error(tree.pos, s"Unknow name in CExp ${name}")
                  Some((cInfo, None))
              }
            } else {
              Some((cInfo, Some(SignalRef(s, cInfo.getCType(s)))))
            }
          } else {
            Some((cInfo, Some(SSelect(s, EmptyMType)))) // SSelect has no SignalInfo
          }
        case _ =>
          unprocessedTree(tree, "SelectLoader")
          None
      }

    }

  }

}
