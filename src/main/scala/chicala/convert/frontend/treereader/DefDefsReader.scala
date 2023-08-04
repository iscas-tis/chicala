package chicala.convert.frontend

import scala.tools.nsc.Global

trait DefDefsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object DefDefReader {
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
              val (newCInfo, vpss: List[List[MValDef]]) =
                vparamss.foldLeft((cInfo, List.empty[List[MValDef]])) { case ((cf, ls), vps) =>
                  val (ncf, nl) = vps.foldLeft((cf, List.empty[MValDef])) { case ((c, l), t) =>
                    ValDefReader(c, t) match {
                      case Some((nc, Some(svd: MValDef))) => (nc, l :+ svd)
                      case x =>
                        unprocessedTree(t, "DefDefReader vparam")
                        (c, l)
                    }
                  }
                  (ncf, ls :+ nl)
                }
              val body = passThrough(rhs)._1 match {
                case Block(stats, expr) => BlockReader(newCInfo, rhs).get._2.get
                case _ =>
                  val mTerm = MTermLoader(newCInfo, rhs).get._2.get
                  SBlock(List(mTerm), mTerm.tpe)
              }
              assert(body.body.nonEmpty, s"function $name should have body")
              Some((cInfo.updatedFuncion(name, tpt), Some(SDefDef(name, vpss, tpt, body))))
            }
          }
        }
        case _ =>
          unprocessedTree(tree, "DefDefReader")
          None
      }

    }
  }

}
