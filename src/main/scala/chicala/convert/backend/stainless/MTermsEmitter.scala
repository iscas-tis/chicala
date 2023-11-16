package chicala.convert.backend.stainless

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.convert.backend.util._
import chicala.ChicalaConfig

trait MTermsEmitter { self: StainlessEmitter with ChicalaAst =>
  val global: Global
  import global._

  trait MTermEmitterImplicit { self: StainlessEmitterImplicit =>
    implicit class MTermEmitter(mTerm: MTerm) {
      def toCode: String = toCode(false)
      def toCode(isLeftSide: Boolean): String = mTerm match {
        case s: SignalRef       => signalRefCode(s, isLeftSide)
        case c: CApply          => cApplyCode(c)
        case l: Lit             => litCode(l)
        case a: Assert          => assertCode(a)
        case s: SApply          => sApplyCode(s)
        case s: SSelect         => sSelectCode(s)
        case s: STuple          => sTupleCode(s)
        case s: SLib            => sLibCode(s)
        case s: SFunction       => s.toCodeLines.toCode
        case s: SIf             => s"(${s.toCodeLines.toCode})"
        case SIdent(name, _)    => name.toString()
        case SLiteral(value, _) => value.toString()
        case _                  => s"TODO(Code ${mTerm})"
      }
      def toCodeLines: CodeLines = mTerm match {
        case _: SignalRef | _: CApply | _: Assert | _: STuple | _: SLiteral | _: SApply | _: SIdent | _: SSelect |
            _: Lit | _: SLib =>
          CodeLines(mTerm.toCode)
        case c: Connect      => connectCL(c)
        case w: When         => whenCL(w, false)
        case s: Switch       => switchCL(s)
        case s: SubModuleRun => subModuleRunCL(s)
        case s: SIf          => sIfCL(s)
        case s: SBlock       => sBlockCL(s)
        case s: SFunction    => sFunctionCL(s)
        case s: SAssign      => sAssignCL(s)
        case EmptyMTerm      => CodeLines("()")
        case _               => CodeLines(s"TODO(CL ${mTerm})")
      }

      private def signalRefCode(signalRef: SignalRef, isLeftSide: Boolean = false): String = {
        def getNameFromTree(tree: Tree): String = {
          tree match {
            case Ident(name)             => name.toString()
            case Select(This(_), name)   => name.toString()
            case Select(qualifier, name) => s"${getNameFromTree(qualifier)}_${name}"
            case _                       => s"TODO(signalRefCode ${tree})"
          }
        }
        val baseName = getNameFromTree(signalRef.name)

        if (signalRef.tpe.isReg) {
          if (isLeftSide) baseName + "_next"
          else "regs." + baseName
        } else if (signalRef.tpe.isInput) {
          "inputs." + baseName
        } else {
          baseName
        }
      }
      private def cApplyCode(cApply: CApply): String = {
        val operands = cApply.operands.map(_.toCode)
        val op = cApply.op match {
          // CBinaryOp
          case Add       => "+"
          case Minus     => "-"
          case Multiply  => "*"
          case And       => "&"
          case Or        => "|"
          case Xor       => "^"
          case LShift    => "<<"
          case RShift    => ">>"
          case Equal     => "==="
          case NotEqual  => "=/="
          case GreaterEq => ">="
          case LogiAnd   => "&&"
          case LogiOr    => "||"
          // CFrontOp
          case LogiNot  => "!"
          case Not      => "~"
          case Negative => "-"
          // CBackOp
          case Slice     => ""
          case VecSelect => ""
          case VecTake   => ".take"
          case VecLast   => ".last"
          case AsUInt    => ".asUInt"
          case AsSInt    => ".asSInt"
          case AsBool    => ".asBool"
          // CUtilOp
          case Mux       => "Mux"
          case MuxLookup => "MuxLookup"
          case Cat       => "Cat"
          case Fill      => "Fill"
          case Log2      => "Log2"
        }
        cApply.op match {
          case b: CBinaryOp => s"(${operands(0)} ${op} ${operands(1)})"
          case f: CFrontOp  => s"${op}${operands(0)}"
          case b: CBackOp =>
            operands match {
              case Nil          => s"FIXME(${cApply})"
              case head :: Nil  => s"${head}${op}"
              case head :: tail => s"${head}${op}(${tail.mkString(", ")})"
            }
          case u: CUtilOp =>
            if (u == Cat && operands.size > 2)
              s"${op}(List(${operands.mkString(", ")}))"
            else
              s"${op}(${operands.mkString(", ")})"
          case _ => s"TODO(${cApply})"
        }
      }
      private def litCode(lit: Lit): String = {
        val literal = lit.litExp.toCode
        val tpe = lit.tpe match {
          case _: UInt => "U"
          case _: SInt => "S"
          case _: Bool => "B"
        }
        (lit.tpe match {
          case _: Bool                          => InferredSize
          case UInt(width, physical, direction) => width
          case SInt(width, physical, direction) => width
        }) match {
          case KnownSize(width) => s"Lit(${literal}, ${width.toCode}).${tpe}"
          case _                => s"Lit(${literal}).${tpe}"
        }
      }
      private def assertCode(assert: Assert): String = {
        s"Assert(${assert.exp.toCode})"
      }
      private val symbolList = List(
        "$plus"    -> "+",
        "$minus"   -> "-",
        "$times"   -> "*",
        "$div"     -> "/",
        "$percent" -> "%",
        "$eq"      -> "=",
        "$greater" -> ">",
        "$less"    -> "<",
        "$bang"    -> "!",
        "$amp"     -> "&",
        "$bar"     -> "|",
        "$colon"   -> ":"
      )
      private def sApplyCode(sApply: SApply): String = {
        val args = sApply.args.map(_.toCode).mkString(", ")
        sApply.fun match {
          case SSelect(fromTmp, nameTmp, tpe) =>
            val from = fromTmp.toCode
            val name = symbolList
              .foldLeft(nameTmp.toString())({ case (s, (a, b)) =>
                s.replace(a, b)
              })

            if (
              "+ - * / % < > == >= <= != << && || -> ++ :+ until"
                .split(" ")
                .contains(name)
            )
              s"(${from} ${name} ${args})"
            else {
              name match {
                case "max"      => s"max(${from}, ${args})"
                case "apply"    => s"${from}(${args})"
                case "map"      => s"${from}.map(${args})"
                case "foreach"  => s"${from}.foreach(${args})"
                case "scanLeft" => s"${from}.scanLeft(${args})"
                case "take"     => s"${from}.take(${args})"
                case "drop"     => s"${from}.drop(${args})"
                case "forall"   => s"${from}.forall(${args})"
                case "+:"       => s"(${args} +: ${from})"
                case "update"   => s"${from} = ${from}.updated(${args})"
                case _ =>
                  s"(${from} TODO(sApplyCode SSelect ${name}) ${args})"
              }
            }
          case SLib(name, tpe) =>
            name match {
              case "scala.`package`.BigInt.apply" | "scala.Predef.intWrapper" | "math.this.BigInt.int2bigInt" |
                  "scala.Predef.ArrowAssoc" | "scala.Predef.refArrayOps" =>
                args

              case "chisel3.util.log2Up.apply"    => s"log2Up(${args})"
              case "chisel3.util.log2Ceil.apply"  => s"log2Ceil(${args})"
              case "chisel3.util.log2Floor.apply" => s"log2Floor(${args})"

              case "scala.`package`.Range.apply" => s"Range(${args})"
              case "scala.`package`.Seq.apply" =>
                val SeqTpe = if (ChicalaConfig.simulation) "Seq" else "List"
                sApply.args match {
                  case Nil          => s"${SeqTpe}[${sApply.tpe.asInstanceOf[StSeq].tparam.toCode}]()"
                  case head :: next => s"${SeqTpe}(${args})"
                }

              case "scala.Array.fill" => s"List.fill(${args})"

              case _ => s"TODO(sApplyCode SLib ${name}(${args}))"
            }
          case SIdent(name, tpe) =>
            s"${name.toString()}(${args})"
          case s: SApply =>
            if (args.startsWith("ClassTag")) s"${s.toCode}"
            else s"${s.toCode}(${args})"
          case _ => s"TODO(sApplyCode ${sApply.fun.toCode}(${args}))"
        }

      }
      private def sSelectCode(sSelect: SSelect): String = {
        val from = sSelect.from.toCode
        val name = sSelect.name.toString()

        (
          Map(
            "getWidth"     -> "width",
            "head"         -> "head",
            "tail"         -> "tail",
            "last"         -> "last",
            "zipWithIndex" -> "zipWithIndex",
            "size"         -> "size",
            "length"       -> "length",
            "reverse"      -> "reverse",
            "nonEmpty"     -> "nonEmpty"
          ) ++
            ((1 until 22).map(i => s"_${i}" -> s"_${i}"))
        ).get(name)
          .map(func => s"${from}.${func}")
          .getOrElse(
            name match {
              case "bitLength"    => s"bitLength(${from})"
              case "indices"      => s"(0 until ${from}.length)"
              case "toIndexedSeq" => s"${from}"
              case _              => s"TODO(SSelect ${name} ${sSelect})"
            }
          )
      }
      private def sTupleCode(sTuple: STuple): String = {
        s"(${sTuple.args.map(_.toCode).mkString(", ")})"
      }
      private def sLibCode(sLib: SLib): String = {
        sLib.name match {
          case "scala.`package`.Nil" => "Nil"

          case _ => s"TODO(sLibCode ${sLib})"
        }
      }
      private def connectCL(cnt: Connect): CodeLines = {
        cnt.expands
          .map(connect =>
            connect.left match {
              case CApply(VecSelect, tpe, operands) =>
                val left = operands.head.toCode(true)
                val idx  = operands.tail.head.toCode
                val expr = connect.expr.toCodeLines
                CodeLines.warpToOneLine(
                  s"${left} = ${left}.updated(${idx}, ",
                  expr.indented,
                  ")"
                )
              case SignalRef(_, _) =>
                val left = connect.left.toCode(true)
                val expr = connect.expr.toCodeLines
                if (expr.lines.head.startsWith("if"))
                  CodeLines.warpToOneLine(
                    s"${left} = ${left} := (",
                    expr.indented,
                    ")"
                  )
                else
                  s"${left} = ${left} := ".concatLastLine(expr)
              case _ =>
                CodeLines(s"Unsupport(${connect})")
            }
          )
          .toCodeLines
      }
      private def whenCL(when: When, isElseWhen: Boolean): CodeLines = {
        val ifword = if (isElseWhen) " else if" else "if"
        val whenPart = CodeLines
          .warpToOneLine(s"${ifword} (when(", when.cond.toCodeLines, ")) ")
          .concatLastLine(when.whenp.toCodeLines)
        val otherPart: CodeLines = {
          if (when.hasElseWhen) {
            val elseWhen = when.otherp.asInstanceOf[When]
            whenCL(elseWhen, true)
          } else {
            val other = when.otherp.toCodeLines
            if (when.otherp.isEmpty) CodeLines.empty
            else CodeLines(" else ").concatLastLine(other)
          }
        }
        whenPart.concatLastLine(otherPart)
      }
      private def switchCL(switch: Switch): CodeLines = {
        val signal = switch.cond.toCode
        val branchs = switch.branchs.map { case (value, branchp) =>
          CodeLines(s"if (${signal} == ${value.toCode}) ")
            .concatLastLine(branchp.toCodeLines)
        }
        branchs.reduceRight((a, b) => a.concatLastLine(" else ".concatLastLine(b)))
      }
      private def subModuleRunCL(subModuleRun: SubModuleRun): CodeLines = {
        val Select(t, n)   = subModuleRun.name.asInstanceOf[Select]
        val moduelFullName = subModuleRun.moduleType.fullName
        val inputs = CodeLines.warpToOneLine(
          s"${moduelFullName}Inputs(",
          subModuleRun.inputRefs
            .map(_.toCode)
            .toCodeLines
            .enddedWithExceptLast(","),
          ")"
        )
        val regs = s"${moduelFullName}Regs()"

        val outputName = s"${n}TransOutputs"

        CodeLines(
          s"val (${outputName}, _) = ${n}.trans(",
          CodeLines(
            inputs.concatLastLine(","),
            regs
          ).indented,
          ")"
        ) ++ (
          subModuleRun.outputSignals
            .map({ case (name, _) => s"${n}_${name} = ${outputName}.${name}" })
            .toCodeLines
        )
      }
      private def sIfCL(sIf: SIf): CodeLines = {
        val cond  = sIf.cond.toCode
        val thenp = sIf.thenp.toCodeLines
        val elsep = sIf.elsep.toCodeLines
        CodeLines
          .warpToOneLine("if (", cond, ") ")
          .concatLastLine(thenp)
          .concatLastLine({
            if (sIf.elsep.isEmpty) CodeLines.empty
            else CodeLines(" else ").concatLastLine(elsep)
          })
      }
      private def sBlockCL(sBlock: SBlock): CodeLines = {
        CodeLines.warpToNewLine(
          "{",
          sBlock.body.map(_.toCodeLines).toCodeLines.indented,
          "}"
        )
      }
      private def sFunctionCL(sFunction: SFunction): CodeLines = {
        sFunction.funcp match {
          case SMatch(selector, head :: Nil, tpe) =>
            val tuple = head.tupleNames.map(_._1.toString()).mkString(", ")
            val body  = head.casep.toCodeLines
            if (body.lines.head == "{")
              CodeLines(
                s"{ case (${tuple}) => {",
                CodeLines(body.lines.tail.init).indented,
                "}}"
              )
            else
              CodeLines.warpToNewLine(
                s"{ case (${tuple}) =>",
                body.indented,
                "}"
              )
          case x =>
            val param = sFunction.vparams.map(_.toCode_param).mkString(", ")
            CodeLines(s"(${param}) => ").concatLastLine(
              x.toCodeLines
            )
        }
      }
      private def sAssignCL(sAssign: SAssign): CodeLines = {
        val left  = sAssign.lhs.toCode(true)
        val right = sAssign.rhs.toCodeLines

        CodeLines(s"${left} = ").concatLastLine(right)
      }
    }
  }
}
