package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst

trait SubModuleCalls extends ChicalaPasss { self: ChicalaAst =>
  val global: Global
  import global._

  object SubModuleCall extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef => subModuleCall(m)
        case x            => x
      }
    }

    def subModuleCall(moduleDef: ModuleDef): ModuleDef = {
      val newBody = processMStatements(moduleDef.body, Map.empty)(moduleDef.name)
      moduleDef.copy(body = newBody)
    }

    private def processMStatements(
        body: List[MStatement],
        replaceMap: Map[String, MStatement]
    )(implicit moduleName: TypeName): List[MStatement] = {
      var repMap = Map.empty[String, MStatement]
      body
        .map({
          case s @ SubModuleDef(name, tpe, args) =>
            val (ioSigDefs, newRepMap) = ioSignalDefs(name, tpe.ioDef.name, tpe.ioDef.tpe)
            repMap = repMap ++ newRepMap
            List(s) ++ ioSigDefs
          case x =>
            List(x.replaced(repMap))
        })
        .flatten
    }

    private def ioSignalDefs(
        subModuleName: TermName,
        ioName: TermName,
        ioType: SignalType
    )(implicit moduleName: TypeName): (List[MStatement], Map[String, MStatement]) = {
      val signals       = ioType.flatten(ioName.toString())
      val inputSignals  = signals.filter({ case (name, tpe) => tpe.isInput })
      val outputSignals = signals.filter({ case (name, tpe) => tpe.isOutput })

      val inputDefs = inputSignals.map({ case (name, tpe) =>
        WireDef(TermName(s"${subModuleName}_${name}"), tpe.updatedPhysical(Wire), None)
      })

      val outputName = s"${subModuleName.toString()}_outputs"
      val subModuleRun = SubModuleRun(
        Select(This(moduleName), subModuleName),
        inputSignals,
        outputSignals,
        outputName
      )

      val outputDefs = outputSignals.map({ case (name, tpe) =>
        val nodeType = tpe.updatedPhysical(Node)
        NodeDef(
          TermName(s"${subModuleName}_${name}"),
          nodeType,
          SignalRef(Select(Select(This(moduleName), outputName), name), nodeType)
        )
      })

      def selectIt(selectPath: List[TermName]): (List[Tree], String) = {
        val selects = selectPath match {
          case head :: next =>
            List(Select(This(moduleName), head), Ident(head))
              .map(x => next.foldLeft(x)((x, y) => Select(x, y)))
          case Nil => List.empty
        }
        val flattenName = selectPath.mkString("_")
        (selects, flattenName)
      }
      def getReplaceMap(tpe: SignalType, prefixs: List[TermName]): Map[String, MStatement] = {
        tpe match {
          case _: GroundType | _: Vec =>
            val (selects, flattenName) = selectIt(prefixs)
            val newType =
              if (tpe.isInput) tpe.updatedPhysical(Wire)
              else tpe.updatedPhysical(Node)
            selects
              .map(x =>
                SignalRef(x, tpe).toString()
                  -> SignalRef(Select(This(moduleName), flattenName), newType)
              )
              .toMap
          case Bundle(physical, signals) =>
            signals
              .map({ case (name, tpe) => getReplaceMap(tpe, prefixs :+ name) })
              .reduce(_ ++ _)
        }
      }
      val replaceMap = getReplaceMap(ioType, List(subModuleName, ioName))

      (inputDefs ++ List(subModuleRun) ++ outputDefs, replaceMap)
    }

  }
}
