package chicala.ast

import scala.tools.nsc.Global

trait CClassDefs { self: ChicalaAst =>
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
  ) extends CClassDef(name, pkg) {}

  case class BundleDef(name: TypeName, bundle: Bundle, pkg: String) extends CClassDef(name, pkg) {}
}
