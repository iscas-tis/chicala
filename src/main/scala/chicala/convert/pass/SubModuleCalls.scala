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
            val (ioSigDefs, newRepMap) = ioSignalDefs(name, tpe, tpe.ioDef.name, tpe.ioDef.tpe)
            repMap = repMap ++ newRepMap
            List(s) ++ ioSigDefs
          case x =>
            List(x.replaced(repMap))
        })
        .flatten
    }

    private def ioSignalDefs(
        subModuleName: TermName,
        subModuleType: SubModule,
        ioName: TermName,
        ioType: SignalType
    )(implicit moduleName: TypeName): (List[MStatement], Map[String, MStatement]) = {
      def flattenName(name: String) = s"${subModuleName}_${name}"

      val signals       = ioType.flatten(ioName.toString())
      val inputSignals  = signals.filter({ case (name, tpe) => tpe.isInput })
      val outputSignals = signals.filter({ case (name, tpe) => tpe.isOutput })

      val inputDefs = inputSignals.map({ case (name, tpe) =>
        WireDef(
          TermName(s"${subModuleName}_${name}"),
          tpe.updatedPhysical(Wire).updatedDriction(Undirect),
          None
        )
      })
      val inputRefs = inputSignals.map({ case (name, tpe) =>
        SignalRef(
          Select(This(moduleName), flattenName(name)),
          tpe.updatedPhysical(Wire).updatedDriction(Undirect)
        )
      })

      val outputNames = outputSignals.map({ case (name, _) =>
        TermName(flattenName(name))
      })

      val subModuleRun = SubModuleRun(
        Select(This(moduleName), subModuleName),
        inputRefs,
        outputNames,
        subModuleType,
        inputSignals,
        outputSignals
      )

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
              if (tpe.isInput) tpe.updatedPhysical(Wire).updatedDriction(Undirect)
              else tpe.updatedPhysical(Node).updatedDriction(Undirect)
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

      (inputDefs ++ List(subModuleRun), replaceMap)
    }

  }
}
