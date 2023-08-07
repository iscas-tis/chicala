package chicala.ast

import scala.tools.nsc.Global

trait CClassDefs { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CClassDef {
    def convert: Tree
  }

  case class ModuleDef(
      name: TypeName,
      vparams: List[SValDef],
      body: List[MStatement]
  ) extends CClassDef {
    def convert: Tree = {
      // TODO
      Literal(Constant(()))
    }
  }

  case class BundleDef(name: TypeName, bundle: Bundle) extends CClassDef {
    def convert: Tree = {
      // TODO
      Literal(Constant(()))
    }
  }
}
