package chicala.convert.frontend

import scala.tools.nsc.Global

trait MatchsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object MatchReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SMatch])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Match(selector, cases) =>
          val mTerm = MTermLoader(cInfo, selector).get._2.get
          val tpe   = MTypeLoader(tpt).get
          val cs = cases.map({ case CaseDef(pat, guard, body) =>
            val nameTypes = pat match {
              case Apply(t: TypeTree, args) =>
                // assume `t` is some tuple
                args.map { case b @ Bind(name: TermName, body) =>
                  (name, MTypeLoader(TypeTree(b.tpe)).get)
                }
              case x =>
                unprocessedTree(x, "MatchReader cases")
                List.empty
            }
            val newCInfo = nameTypes.foldLeft(cInfo) { case (cf, (name, mType)) =>
              cf.updatedVal(name, mType)
            }
            val mBody = MTermLoader(newCInfo, body).get._2.get

            SCaseDef(nameTypes, mBody, MTypeLoader(tpt).get)
          })
          Some((cInfo, Some(SMatch(mTerm, cs, tpe))))

        case _ =>
          unprocessedTree(tree, "MatchReader")
          None
      }
    }

  }

}
