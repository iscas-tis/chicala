package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst
import chicala.ast.util._

trait SubModuleCalls extends ChicalaPasss with Transformers with Replacers { self: ChicalaAst =>
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
      moduleDef.copy(body = expandTopList(moduleDef.body)(moduleDef.name))
    }

    def expandTopList(body: List[MStatement])(implicit moduleName: TypeName): List[MStatement] = {
      var repMap = Map.empty[MStatement, MStatement]
      body
        .map({
          Replacer(repMap)(_) match {
            case s @ SubModuleDef(name, tpe, args) =>
              val (ioSigDefs, subModuleRun, newRepMap) = expand(s)
              repMap = repMap ++ newRepMap
              (s :: ioSigDefs) :+ subModuleRun
            case x =>
              val subModuleExpander = new SubModuleExpander()
              val newStatement      = subModuleExpander(Replacer(repMap)(x))
              subModuleExpander.pullOutDefs :+ newStatement
          }
        })
        .flatten
    }

    private def expand(
        subModuleDef: SubModuleDef
    )(implicit moduleName: TypeName): (List[MStatement], SubModuleRun, Map[MStatement, MStatement]) = {
      val SubModuleDef(name, tpe, args) = subModuleDef
      val subModuleName                 = name
      val subModuleType                 = tpe
      val ioName                        = tpe.ioDef.name
      val ioType                        = tpe.ioDef.tpe

      def flattenName(name: String) = s"${subModuleName}_${name}"

      val signals       = ioType.flatten(ioName.toString())
      val inputSignals  = signals.filter({ case (name, tpe) => tpe.isInput })
      val outputSignals = signals.filter({ case (name, tpe) => tpe.isOutput })

      val ioSigDefs = signals.map({ case (name, tpe) =>
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
      def getReplaceMap(tpe: SignalType, prefixs: List[TermName]): Map[MStatement, MStatement] = {
        tpe match {
          case _: GroundType | _: Vec =>
            val (selects, flattenName) = selectIt(prefixs)
            val newType =
              if (tpe.isInput) tpe.updatedPhysical(Wire).updatedDriction(Undirect)
              else tpe.updatedPhysical(Node).updatedDriction(Undirect)
            selects
              .map(x =>
                SignalRef(x, tpe)
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

      (ioSigDefs, subModuleRun, replaceMap)
    }

    class SubModuleExpander()(implicit moduleName: TypeName) extends Transformer {
      var pullOutDefs: List[MStatement] = List.empty

      override def transform(mStatement: MStatement): MStatement = mStatement match {
        case d @ SDefDef(_, _, _, defp) => d.copy(defp = expandTopSBlockOrOther(defp))
        case f @ SFunction(_, funcp)    => f.copy(funcp = expandTopSBlockOrOther(funcp).asInstanceOf[MTerm])
        case b @ SBlock(body, tpe)      => b.copy(body = expandSubList(body))
        case x                          => super.transform(x)
      }

      private def expandTopSBlockOrOther(bodyp: MStatement): MStatement = {
        bodyp match {
          case SBlock(body, tpe) => SBlock(expandTopList(body), tpe)
          case x                 => transform(x)
        }
      }

      private def expandSubList(body: List[MStatement]): List[MStatement] = {
        var repMap = Map.empty[MStatement, MStatement]
        body.map({
          Replacer(repMap)(_) match {
            case s @ SubModuleDef(name, tpe, args) =>
              val (ioSigDefs, subModuleRun, newRepMap) = expand(s)
              pullOutDefs = pullOutDefs ++ (s :: ioSigDefs)
              repMap = repMap ++ newRepMap
              subModuleRun
            case x => super.transform(x)
          }
        })
      }

    }

  }
}
