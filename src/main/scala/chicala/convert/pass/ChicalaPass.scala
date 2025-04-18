package chicala.convert.pass

import chicala.ast.ChicalaAst

trait ChicalaPasss { self: ChicalaAst =>

  trait ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef
  }

  object RunChicalaPass {
    def apply(cClassDef: CClassDef, passs: List[ChicalaPass]): CClassDef = {
      passs.foldLeft(cClassDef)((a, b) => b(a))
    }
  }
}

trait ChicalaPassCollecttion
    extends DependencySorts
    with LiteralPropagations
    with RegEnableApplys
    with SubModuleCalls
    with ChicalaPeeks {
  self: ChicalaAst =>
}
