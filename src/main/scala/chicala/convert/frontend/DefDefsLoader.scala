package chicala.convert.frontend

import scala.tools.nsc.Global

trait DefDefsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object DefDefLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MDef])] = {
      val tree = passThrough(tr)._1
      tree match {
        case d @ DefDef(mods, name, tparams, vparamss, tpt: TypeTree, rhs) => {
          name match {
            // constructor of this class
            case termNames.CONSTRUCTOR => {
              // register construct param
              val params = vparamss.flatten.map(x => (x.name, x.tpt.asInstanceOf[TypeTree]))
              Some((cInfo.updatedParams(params), None))
            }
            // accessor of signal
            case t: TermName if cInfo.signal.contains(t) => None
            case _ => {
              val (newCInfo, vpss: List[List[SValDef]]) =
                vparamss.foldLeft((cInfo, List.empty[List[SValDef]])) { case ((cf, ls), vps) =>
                  val (ncf, nl) = vps.foldLeft((cf, List.empty[SValDef])) { case ((c, l), t) =>
                    ValDefLoader(c, t) match {
                      case Some((nc, Some(svd: SValDef))) => (nc, l :+ svd)
                      case _ =>
                        unprocessedTree(t, "SDefDefLoader")
                        (c, l)
                    }
                  }
                  (ncf, ls :+ nl)
                }
              val body = BlockLoader(newCInfo, rhs) match {
                case Some((_, Some(value))) => value
                case _                      => SBlock(List.empty, EmptyMType)
              }
              assert(body.body.nonEmpty, s"function $name should have body")
              Some((cInfo.updatedFuncion(name, tpt), Some(SDefDef(name, vpss, tpt, body))))
            }
          }
        }
        case _ =>
          unprocessedTree(tree, "DefDefLoader")
          None
      }

    }
  }

}
