package chicala.convert.frontend

import scala.tools.nsc.Global

trait SStatementsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object SDefDefLoader extends MStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SDefDef])] = {
      val tree = passThrough(tr)._1
      tree match {
        case DefDef(mods, name, tparams, vparamss, tpt: TypeTree, rhs) =>
          val (newCInfo, vpss: List[List[SValDef]]) =
            vparamss.foldLeft((cInfo, List.empty[List[SValDef]])) { case ((cf, ls), vps) =>
              val (ncf, nl) = vps.foldLeft((cf, List.empty[SValDef])) { case ((c, l), t) =>
                SValDefLoader(c, t) match {
                  case Some((nc, Some(svd))) => (nc, l :+ svd)
                  case _ =>
                    unprocessedTree(t, "SDefDefLoader")
                    (c, l)
                }
              }
              (ncf, ls :+ nl)
            }
          val body = SBlockLoader(newCInfo, rhs) match {
            case Some((_, Some(value))) => value
            case _                      => SBlock.empty
          }
          assert(body.body.nonEmpty, s"function $name should have body")
          Some((cInfo.updatedFuncion(name, tpt), Some(SDefDef(name, vpss, tpt, body))))
        case _ => None
      }
    }
  }
  object SValDefLoader extends MStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SValDef])] = {
      val tree = passThrough(tr)._1
      tree match {
        case ValDef(mods, name, tpt, rhs) =>
          val newCInfo =
            if (isChiselType(tpt))
              cInfo.updatedSignal(name, CTypeLoader(tpt))
            else
              cInfo.updatedParam(name, tpt.asInstanceOf[TypeTree])
          Some((newCInfo, Some(SValDef(name, STypeLoader(tpt), CExpLoader(cInfo, rhs))))) // ? or SExp?
        case _ => None
      }
    }
  }
  object SApplyLoader extends MStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SApply])] = {
      val tree = passThrough(tr)._1
      tree match {
        case Apply(fun, args) =>
          passThrough(fun)._1 match {
            case s: Select =>
              Some((cInfo, Some(SApply(SSelect(s, EmptyMType), args.map(CExpLoader(cInfo, _)), EmptyMType))))
            case _ => None
          }
        case _ => None
      }
    }
  }
  object SBlockLoader extends MStatementObj {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SBlock])] = {
      val tree = passThrough(tr)._1
      tree match {
        case Block(stats, expr) =>
          val (newCInfo, cList) = MStatementLoader.fromListTree(cInfo, stats :+ expr)
          Some((newCInfo, Some(SBlock(cList, EmptyMType))))
        case _ =>
          Some((cInfo, Some(SBlock(List(CExpLoader(cInfo, tr)), EmptyMType))))
      }
    }
  }

}
