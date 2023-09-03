package chicala.ast

import scala.tools.nsc.Global

import chicala.ast.impl.CClassDefsImpl

trait CClassDefs extends CClassDefsImpl { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CClassDef(name: TypeName, pkg: String) {
    def fullName: String = pkg + "." + name
  }

  case class ModuleDef(
      name: TypeName,
      vparams: List[SValDef],
      body: List[MStatement],
      pkg: String
  ) extends CClassDef(name, pkg)
      with ModuleDefImpl

  case class BundleDef(
      name: TypeName,
      vparams: List[SValDef],
      bundle: Bundle,
      pkg: String
  ) extends CClassDef(name, pkg)
      with BundleDefImpl
}
