package chicala.convert.frontend

import scala.tools.nsc.Global

trait SelectsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object SelectReader {
    val sLibs: List[String] = List("scala.`package`.BigInt.apply")
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
                  Some((cInfo, Some(CApply(op, CTypeLoader(tpt), List(MTermLoader(cInfo, qualifier).get._2.get)))))
                case None =>
                  if (isChiselType(s))
                    Some((cInfo, Some(SignalRef(s, cInfo.getCType(s)))))
                  else {
                    reporter.error(tree.pos, s"Unknow op name in CExp ${name}")
                    None
                  }
              }
            } else if (isChiselLiteralType(qualifier)) {
              val litTree = qualifier.asInstanceOf[Apply].args.head
              val litExp  = MTermLoader(cInfo, litTree).get._2.get.asInstanceOf[STerm]

              name.toString() match {
                case "U" => Some((cInfo, Some(Lit(litExp, UInt(InferredSize, Node, Undirect)))))
                case "S" => Some((cInfo, Some(Lit(litExp, SInt(InferredSize, Node, Undirect)))))
                case _ =>
                  reporter.error(tree.pos, s"Unknow name in CExp ${name}")
                  None
              }
            } else {
              Some((cInfo, Some(SignalRef(s, cInfo.getCType(s)))))
            }
          } else {
            val tpe = MTypeLoader(tpt)
            qualifier match {
              case This(cInfo.name) => Some((cInfo, Some(SIdent(name, tpe))))
              case Ident(innerName: TermName) =>
                Some((cInfo, Some(SSelect(SIdent(innerName, MTypeLoader(cInfo, qualifier)), name, tpe))))
              case t =>
                Some(
                  (
                    cInfo,
                    Some(SSelect(STermLoader(cInfo, t).get._2.get, name, tpe))
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
