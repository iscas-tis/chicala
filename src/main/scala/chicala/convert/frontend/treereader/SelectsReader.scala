package chicala.convert.frontend

import scala.tools.nsc.Global

trait SelectsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object SelectReader {
    val sLibs: List[String] = List("scala.`package`.BigInt.apply", "chisel3.util.log2Ceil.apply")
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case s @ Select(qualifier, name: TermName) =>
          if (sLibs.contains(s.toString())) {
            Some((cInfo, Some(SLib(s.toString(), StFunc))))
          } else if (isChiselType(tpt)) {
            if (isChiselType(qualifier)) {
              COpLoader(name.toString()) match {
                case Some(op) =>
                  Some((cInfo, Some(CApply(op, CTypeLoader(tpt).get, List(MTermLoader(cInfo, qualifier).get._2.get)))))
                case None =>
                  if (isChiselType(s))
                    Some((cInfo, Some(SignalRef(s, cInfo.getCType(s)))))
                  else {
                    reporter.error(tree.pos, s"Unknow op name in CExp ${name}")
                    None
                  }
              }
            } else if (isChiselLiteralType(qualifier)) {
              LitLoader(cInfo, tr)
            } else {
              Some((cInfo, Some(SignalRef(s, cInfo.getCType(s)))))
            }
          } else {
            val tpe = MTypeLoader(tpt).get
            qualifier match {
              case This(cInfo.name) => Some((cInfo, Some(SIdent(name, tpe))))
              case Ident(innerName: TermName) =>
                Some((cInfo, Some(SSelect(SIdent(innerName, MTypeLoader(cInfo, qualifier).get), name, tpe))))
              case t =>
                Some(
                  (
                    cInfo,
                    Some(SSelect(MTermLoader(cInfo, t).get._2.get, name, tpe))
                  )
                )
            }
          }
        case _ =>
          unprocessedTree(tree, "SelectReader")
          None
      }

    }

  }

}
