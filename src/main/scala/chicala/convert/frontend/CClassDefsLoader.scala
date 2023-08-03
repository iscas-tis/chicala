package chicala.convert.frontend

import scala.tools.nsc.Global

trait CClassDefsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object CClassDef {
    def apply(tree: Tree): Option[CClassDef] = {
      val someModuleDef = ModuleDefLoader(tree)
      val someBundleDef = BundleDefLoader(tree)
      someModuleDef match {
        case Some(_) => someModuleDef
        case None    => someBundleDef
      }
    }
  }

  object ModuleDefLoader {
    def apply(tree: Tree): Option[ModuleDef] = tree match {
      // only Class inherits chisel3.Module directly
      case ClassDef(mods, name, tparams, Template(parents, self, body)) if parents.exists {
            case Select(Ident(TermName("chisel3")), TypeName("Module")) => true
            case _                                                      => false
          } =>
        val (cInfo, cBody): (CircuitInfo, List[CStatement]) =
          CStatementLoader.fromListTree(CircuitInfo(name), body)
        Some(ModuleDef(name, cInfo, cBody))
      case _ => None
    }
  }

  object BundleDefLoader {
    def apply(tree: Tree): Option[BundleDef] = {
      tree match {
        // only Class inherits chisel3.Bundle directly
        case ClassDef(mods, name, tparams, Template(parents, self, body)) if parents.exists {
              case Select(Ident(TermName("chisel3")), TypeName("Bundle")) => true
              case _                                                      => false
            } =>
          val signals = body
            .map {
              case ValDef(mods, name, tpt, rhs) =>
                Some((name.stripSuffix(" "), CTypeLoader(rhs)))
              case _ => None
            }
            .flatten
            .toMap
          Some(BundleDef(name, Bundle(signals)))
        case _ => None
      }
    }
  }
}
