package chicala.convert.frontend

import scala.tools.nsc.Global

trait CClassDefsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object CClassDefLoader {
    def apply(tree: Tree)(implicit readerInfo: ReaderInfo): Option[(ReaderInfo, Option[CClassDef])] = {
      val someModuleDef = ModuleDefLoader(tree)
      val someBundleDef = BundleDefLoader(tree)
      someModuleDef match {
        case Some(_) => someModuleDef
        case None    => someBundleDef
      }
    }
  }

  object ModuleDefLoader {
    def apply(tree: Tree)(implicit readerInfo: ReaderInfo): Option[(ReaderInfo, Option[ModuleDef])] = tree match {
      // only Class inherits chisel3.Module directly
      case ClassDef(mods, name, tparams, Template(parents, self, body)) if parents.exists {
            case Select(Ident(TermName("chisel3")), TypeName("Module")) => true
            case _                                                      => false
          } =>
        val (cInfo, cBody): (CircuitInfo, List[MStatement]) =
          StatementReader.fromListTree(CircuitInfo(name), body)
        Some((cInfo.readerInfo, Some(ModuleDef(name, cInfo.params.toList, cBody))))
      case _ => None
    }
  }

  object BundleDefLoader {
    def apply(tree: Tree)(implicit readerInfo: ReaderInfo): Option[(ReaderInfo, Option[BundleDef])] = {
      BundleDefLoader(CircuitInfo.empty, tree) match {
        case None                         => None
        case Some((cInfo, someBundleDef)) => Some(cInfo.readerInfo, someBundleDef)
      }
    }
    def apply(cInfo: CircuitInfo, tree: Tree): Option[(CircuitInfo, Option[BundleDef])] = {
      tree match {
        // only Class inherits chisel3.Bundle directly
        case ClassDef(mods, name, tparams, Template(parents, self, body)) if parents.exists {
              case Select(Ident(TermName("chisel3")), TypeName("Bundle")) => true
              case _                                                      => false
            } =>
          val signals = body
            .map {
              case ValDef(mods, name, tpt, rhs) =>
                Some((name.stripSuffix(" "), CTypeLoader(cInfo, rhs)))
              case _ => None
            }
            .flatten
            .toMap
          Some((cInfo, Some(BundleDef(name, Bundle(Node, signals)))))
        case _ => None
      }
    }
  }
}
