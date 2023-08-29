package chicala.convert.backend.stainless

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.convert.backend.util._

trait ModuleDefsEmitter { self: StainlessEmitter with ChicalaAst =>
  val global: Global
  import global._

  trait ModuleDefEmitterImplicit { self: StainlessEmitterImplicit =>

    implicit class ModuleDefEmitter(moduleDef: ModuleDef) {

      def toCode: String = toCodeLines.toCode

      def toCodeLines: CodeLines = {
        val head = CodeLines(
          s"""package ${moduleDef.pkg}
         |
         |import stainless.lang._
         |import stainless.collection._
         |import stainless.equations._
         |import stainless.annotation._
         |import stainless.proof.check
         |
         |import libraryUInt._""".stripMargin
        )

        val inputsCaseClass  = signalsClassCL(inputsClassName, inputSignals)
        val outputsCaseClass = signalsClassCL(outputsClassName, outputSignals)
        val regsCaseClass    = signalsClassCL(regsClassName, regSignals)

        val moduleCaseClass = moduleClassCL

        CodeLines(
          head,
          CodeLines.blank,
          inputsCaseClass,
          outputsCaseClass,
          regsCaseClass,
          CodeLines.blank,
          moduleCaseClass
        )
      }

      private val moduleName       = moduleDef.name.toString()
      private val inputsClassName  = moduleName + "Inputs"
      private val outputsClassName = moduleName + "Outputs"
      private val regsClassName    = moduleName + "Regs"

      private val moduleRunName =
        s"${moduleName.head.toLower}${moduleName.tail}Run"

      private val ioDef = moduleDef.ioDef
      private val inputSignals = ioDef.tpe
        .serialize(ioDef.name.toString())
        .filter { case (name, tpe) => tpe.isInput }
      private val outputSignals = ioDef.tpe
        .serialize(ioDef.name.toString())
        .filter { case (name, tpe) => tpe.isOutput }

      private val regDefs = moduleDef.regDefs
      private val regSignals = regDefs
        .map({ case RegDef(name, tpe, someInit, someNext, someEnable) =>
          tpe.serialize(name.toString())
        })
        .flatten

      private def signalsClassCL(
          signalClassName: String,
          signals: List[(String, SignalType)]
      ): CodeLines = {
        def signalsCL(signals: List[(String, SignalType)]): CodeLines = {
          signals.toList
            .map({
              case (name, tpe) => {
                s"${name}: ${tpe.toCode}"
              }
            })
            .toCodeLines
            .enddedWithExceptLast(",")
        }

        CodeLines.warpToOneLine(
          s"case class ${signalClassName}(",
          signalsCL(signals).indented(2),
          ")"
        )
      }
      private def signalsRequireCL(
          signalGroup: String,
          signalClassName: String,
          signals: List[(String, SignalType)]
      ): CodeLines = {
        def signalRequire(name: String, tpe: SignalType): CodeLines = {
          tpe match {
            case cType: CType =>
              cType match {
                case UInt(width: KnownSize, _, _) =>
                  CodeLines(s"${name}.width == ${width.width.toCode}")
                case Bool(_, _) => CodeLines.empty
                case Vec(size: KnownSize, _, tpe) =>
                  CodeLines(s"${name}.length == ${size.width.toCode}") ++
                    (tpe match {
                      case UInt(width: KnownSize, _, _) =>
                        CodeLines(s"${name}.forall(_.width == ${width.width.toCode})")
                      case _ => CodeLines.empty
                    })
                case _ => CodeLines(s"// Unknown size ${name}")
              }
            case _ => CodeLines(s"FIXME(${name})")
          }
        }

        val signalNames = signals.map(_._1).mkString(", ")

        val requires = {
          val tmp = signals
            .map({ case (name, tpe) => signalRequire(name, tpe) })
            .toCodeLines
            .enddedWithExceptLast(" &&")
          if (tmp.isEmpty) CodeLines("true") else tmp
        }

        CodeLines(
          s"""def ${signalGroup}Require(${signalGroup}: ${signalClassName}): Boolean = ${signalGroup} match {
             |  case ${signalClassName}(${signalNames}) =>""".stripMargin,
          requires.indented(2),
          "}"
        )

      }

      private def moduleClassCL: CodeLines = {
        val vparams = moduleDef.vparams
          .map(_.toCode_param)
          .toCodeLines
          .enddedWithExceptLast(",")

        val inputsRequire  = signalsRequireCL("inputs", inputsClassName, inputSignals)
        val outputsRequire = signalsRequireCL("outputs", outputsClassName, outputSignals)
        val regsRequire    = signalsRequireCL("regs", regsClassName, regSignals)

        val trans = transCL
        val moduleRun = CodeLines(
          s"""def ${moduleRunName}(timeout: Int, inputs: ${inputsClassName}, regInit: ${regsClassName}): (${outputsClassName}, ${regsClassName}) = {
             |  require(timeout >= 1 && inputsRequire(inputs) && regsRequire(regInit))
             |  val (newOutputs, newRegs) = trans(inputs, regInit)
             |  if (timeout > 1) {
             |    ${moduleRunName}(timeout - 1, inputs, newRegs)
             |  } else {
             |    (newOutputs, newRegs)
             |  }
             |} ensuring { case (outputs, regNexts) =>
             |  outputsRequire(outputs) && regsRequire(regNexts)
             |}""".stripMargin
        )
        val run = {
          val regInit: String = CodeLines
            .warpToOneLine(
              s"${regsClassName}(",
              CodeLines.empty.indented,
              ")"
            )
            .toCode
          CodeLines(
            s"""def run(inputs: ${inputsClassName}, randomInitValue: ${regsClassName}): (${outputsClassName}, ${regsClassName}) = {
             |  require(inputsRequire(inputs) && regsRequire(randomInitValue))
             |  val regInit = ${regInit}
             |  ${moduleRunName}(100, inputs, regInit)
             |} ensuring { case (outputs, regNexts) =>
             |  outputsRequire(outputs) && regsRequire(regNexts)
             |}""".stripMargin
          )
        }

        CodeLines(
          CodeLines.warpToOneLine(s"case class ${moduleName}(", vparams.indented(2), ") {"),
          CodeLines(
            inputsRequire,
            outputsRequire,
            regsRequire,
            CodeLines.blank,
            trans,
            CodeLines.blank,
            moduleRun,
            run
          ).indented,
          "}"
        )
      }

      private def transCL: CodeLines = {
        val outputInit = CodeLines(
          "// output",
          outputSignals.map { case (name, tpe) => tpe.toCode_init(name) }.toCodeLines
        )
        val regNextInit = {
          val init = regSignals.map { case (name, tpe) =>
            tpe.toCode_regNextInit(name)
          }.toCodeLines
          if (init.isEmpty) CodeLines.empty
          else
            CodeLines(
              "// reg next",
              init
            )
        }
        val body = CodeLines(
          "// body",
          moduleDef.body.map(_.toCodeLines).toCodeLines
        )

        val returnValue = CodeLines.warpToOneLine(
          "(",
          CodeLines(
            CodeLines.warpToOneLine(
              s"${outputsClassName}(",
              outputSignals
                .map(_._1)
                .toCodeLines
                .enddedWithExceptLast(",")
                .indented,
              "),"
            ),
            CodeLines.warpToOneLine(
              s"${regsClassName}(",
              regSignals
                .map(_._1 + "_next")
                .toCodeLines
                .enddedWithExceptLast(",")
                .indented,
              ")"
            )
          ).indented,
          ")"
        )

        CodeLines(
          s"def trans(inputs: ${inputsClassName}, regs: ${regsClassName}): (${outputsClassName}, ${regsClassName}) = {",
          CodeLines(
            "require(inputsRequire(inputs) && regsRequire(regs))",
            CodeLines.blank,
            outputInit,
            regNextInit,
            CodeLines.blank,
            body,
            CodeLines.blank,
            returnValue
          ).indented,
          s"""} ensuring { case (outputs, regNexts) =>
             |  outputsRequire(outputs) && regsRequire(regNexts)
             |}""".stripMargin
        )
      }
    }

  }
}
