package chicala.convert.backend.stainless

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.convert.backend.util._

trait StainlessEmitter
    extends ModuleDefsEmitter
    with MStatementsEmitter
    with MTermsEmitter
    with MDefsEmitter
    with MTypesEmitter {
  self: ChicalaAst =>

  val global: Global
  import global._

  trait StainlessEmitterImplicit
      extends ModuleDefEmitterImplicit
      with MStatementEmitterImplicit
      with MTermEmitterImplicit
      with MDefEmitterImplicit
      with MTypeEmitterImplicit
      with CodeLinesImplicit

  object EmitStainless extends StainlessEmitterImplicit {
    def apply(moduleDef: ModuleDef): String = {
      moduleDef.toCode
    }
  }
}
