package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst

trait LiteralPropagations extends ChicalaPasss { self: ChicalaAst =>
  val global: Global
  import global._

  object LiteralPropagation extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef => literalPropagation(m)
        case x            => x
      }
    }

    def literalPropagation(moduleDef: ModuleDef): ModuleDef = {
      import scala.collection.mutable

      val enumLit = mutable.Map.empty[String, Lit]

      val newBody = moduleDef.body.map({
        case e @ EnumDef(names, tpe) =>
          enumLit ++= names.zipWithIndex.map({ case (name, idx) =>
            (name.toString(), Lit(SLiteral(idx, StInt), tpe))
          })
          e
        case r @ RegDef(_, _, Some(SignalRef(Select(This(moduleDef.name), name), _)), _, _) =>
          val someLit = enumLit.get(name.toString())
          someLit match {
            case Some(value) => r.copy(someInit = Some(value))
            case None        => r
          }
        case x => x
      })

      moduleDef.copy(body = newBody)
    }
  }
}
