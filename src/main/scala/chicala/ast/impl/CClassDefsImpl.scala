package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait CClassDefsImpl { self: ChicalaAst =>
  val global: Global
  import global._

  trait ModuleDefImpl { self: ModuleDef =>
    def ioDef: IoDef = {
      val ioDefs = body.collect { case x: IoDef => x }
      ioDefs match {
        case Nil =>
          reportError(NoPosition, "ModuleDef should has a IoDef in body")
          IoDef(TermName(""), SignalType.empty)
        case head :: Nil =>
          head
        case head :: next =>
          reportError(NoPosition, "ModuleDef should has only one IoDef in body")
          head
      }
    }
    def regDefs: List[RegDef] = body.collect { case x: RegDef => x }
  }
}
