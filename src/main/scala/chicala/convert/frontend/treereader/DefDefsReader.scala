package chicala.convert.frontend

import scala.tools.nsc.Global

trait DefDefsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object DefDefReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MDef])] = {
      val tree = passThrough(tr)._1
      tree match {
        case d @ DefDef(mods, nameTmp, tparams, vparamss, tpt: TypeTree, rhs) => {
          val name = nameTmp.stripSuffix(" ")

          // accessor of val and var
          if (mods.hasAccessorFlag) {
            val valName =
              if (name.toString().endsWith("_$eq"))
                TermName(name.toString().dropRight(4))
              else name
            assertError(cInfo.contains(valName), d.pos, "accessor of val should record in cInfo")
            return None
          }

          val (newCInfo, vpss: List[List[MValDef]]) = vparamssReader(cInfo, vparamss)

          if (name == termNames.CONSTRUCTOR) {
            // constructor of this class
            val vps = vpss.flatten.asInstanceOf[List[SValDef]]
            Some((cInfo.updatedParams(vps), None))
          } else {
            // function
            val body = passThrough(rhs)._1 match {
              case Block(stats, expr) => BlockReader(newCInfo, rhs).get._2.get
              case _ =>
                val mTerm = MTermLoader(newCInfo, rhs).get._2.get
                SBlock(List(mTerm), mTerm.tpe)
            }
            assertError(body.body.nonEmpty, rhs.pos, s"function $name should have body")
            val tpe = MTypeLoader.fromTpt(tpt).get
            Some((cInfo.updatedFunc(name, tpe), Some(SDefDef(name, vpss, tpe, body))))
          }
        }
        case _ =>
          unprocessedTree(tree, "DefDefReader")
          None
      }

    }
  }

  protected def vparamssReader(cInfo: CircuitInfo, vparamss: List[List[ValDef]]): (CircuitInfo, List[List[MValDef]]) = {
    vparamss
      .foldLeft((cInfo, List.empty[List[MValDef]])) { case ((cf, ls), vps) =>
        val (ncf, nl) = vps.foldLeft((cf, List.empty[MValDef])) { case ((c, l), t) =>
          ValDefReader(c, t) match {
            case Some((nc, Some(svd: MValDef))) => (nc, l :+ svd)
            case x =>
              unprocessedTree(t, "vparamssReader")
              (c, l)
          }
        }
        (ncf, ls :+ nl)
      }
  }

}
