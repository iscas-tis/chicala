package chicala.ast

import scala.tools.nsc.Global
import org.jline.utils.Signals

trait CClassDefs { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CClassDef {
    def convert: Tree
  }
  object CClassDef {
    def fromTree(tree: Tree): Option[CClassDef] = {
      val someModuleDef = ModuleDef.fromTree(tree)
      val someBundleDef = BundleDef.fromTree(tree)
      someModuleDef match {
        case Some(_) => someModuleDef
        case None    => someBundleDef
      }
    }
  }

  case class ModuleDef(name: TypeName, info: CircuitInfo, body: List[CStatement]) extends CClassDef {
    def convert: Tree = {
      // TODO
      Literal(Constant(()))
    }
  }
  object ModuleDef {
    def fromTree(tree: Tree): Option[ModuleDef] = tree match {
      // only Class inherits chisel3.Module directly
      case ClassDef(mods, name, tparams, Template(parents, self, body)) if parents.exists {
            case Select(Ident(TermName("chisel3")), TypeName("Module")) => true
            case _                                                      => false
          } =>
        val (cInfo, cBody): (CircuitInfo, List[CStatement]) =
          CStatement.fromListTree(new CircuitInfo(name), body)
        Some(ModuleDef(name, cInfo, cBody))
      case _ => None
    }
  }

  case class BundleDef(name: TypeName, bundle: Bundle) extends CClassDef {
    def convert: Tree = {
      // TODO
      Literal(Constant(()))
    }
  }
  object BundleDef {
    def fromTree(tree: Tree): Option[BundleDef] = {
      tree match {
        // only Class inherits chisel3.Bundle directly
        case ClassDef(mods, name, tparams, Template(parents, self, body)) if parents.exists {
              case Select(Ident(TermName("chisel3")), TypeName("Bundle")) => true
              case _                                                      => false
            } =>
          val signals = body
            .map {
              case ValDef(mods, name, tpt, rhs) =>
                Some((name.stripSuffix(" "), CDataType.fromTree(rhs)))
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
