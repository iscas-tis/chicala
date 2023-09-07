package chicala.ast

import scala.tools.nsc.Global

import chicala.ast.impl.CClassDefsImpl

trait CClassDefs extends CClassDefsImpl { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CClassDef {
    def name: TypeName
    def pkg: String

    /** path.to.package.className
      */
    def fullName: String = pkg + "." + name
  }

  case class ModuleDef(
      name: TypeName,
      vparams: List[SValDef],
      body: List[MStatement],
      pkg: String
  ) extends CClassDef
      with ModuleDefImpl

  case class BundleDef(
      name: TypeName,
      vparams: List[SValDef],
      bundle: Bundle,
      pkg: String
  ) extends CClassDef
      with BundleDefImpl
}
