package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst

trait RegEnableApplys extends ChicalaPasss { self: ChicalaAst =>
  val global: Global
  import global._

  object RegEnableApply extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef => regEnableApply(m)
        case x            => x
      }
    }

    def regEnableApply(moduleDef: ModuleDef): ModuleDef = {
      val newBody = moduleDef.body
        .map({
          case r @ RegDef(name, tpe, _, someNext, Some(mTerm)) =>
            val cond      = mTerm
            val signalRef = SignalRef(Select(This(moduleDef.name), name), tpe)
            val connect   = Connect(signalRef, someNext.get)
            List(
              r.copy(someNext = None, someEnable = None),
              When(cond, List(connect), List.empty, false)
            )
          case x => List(x)
        })
        .flatten

      moduleDef.copy(body = newBody)
    }
  }
}
