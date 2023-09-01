package chicala.convert.backend.stainless

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.convert.backend.util._
import java.nio.charset.CoderMalfunctionError

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
        case s: SFunction       => s.toCodeLines.toCode
        case SIdent(name, _)    => name.toString()
        case SLiteral(value, _) => value.toString()
        case _                  => s"TODO(Code ${mTerm})"
      }
      def toCodeLines: CodeLines = mTerm match {
        case _: SignalRef | _: CApply | _: Assert | _: STuple | _: SLiteral | _: SApply | _: SIdent | _: SSelect |
            _: Lit =>
          CodeLines(mTerm.toCode)
        case c: Connect   => connectCL(c)
        case w: When      => whenCL(w, false)
        case s: Switch    => switchCL(s)
        case s: SIf       => sIfCL(s)
        case s: SBlock    => sBlockCL(s)
        case s: SFunction => sFunctionCL(s)
        case EmptyMTerm   => CodeLines.empty
        case _            => CodeLines(s"TODO(CL ${mTerm})")
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
          case MuxLookup => "TODO!!!!!!!!!!!!!"
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
      private def sApplyCode(sApply: SApply): String = {
        val args = sApply.args.map(_.toCode).mkString(", ")
        sApply.fun match {
          case SSelect(fromTmp, nameTmp, tpe) =>
            val from = fromTmp.toCode
            val name = nameTmp.toString()
            (name match {
              case "$plus"      => Some("+")
              case "$minus"     => Some("-")
              case "$times"     => Some("*")
              case "$div"       => Some("/")
              case "$percent"   => Some("%")
              case "$less"      => Some("<")
              case "$greater"   => Some(">")
              case "$eq$eq"     => Some("==")
              case "$bang$eq"   => Some("!=")
              case "$less$less" => Some("<<")
              case "$amp$amp"   => Some("&&")
              case "until"      => Some("until")
              case _            => None
            }).map(op => s"(${from} ${op} ${args})")
              .getOrElse(
                name match {
                  case "max"      => s"max(${from}, ${args})"
                  case "apply"    => s"${from}(${args})"
                  case "scanLeft" => s"${from}.scanLeft(${args})"
                  case _ =>
                    s"(${from} TODO(sApplyCode SSelect ${name}) ${args})"
                }
              )
          case SLib(name, tpe) =>
            name match {
              case "scala.`package`.BigInt.apply" | "scala.Predef.intWrapper" | "math.this.BigInt.int2bigInt" => args

              case "chisel3.util.log2Up.apply"    => s"log2Up(${args})"
              case "chisel3.util.log2Ceil.apply"  => s"log2Ceil(${args})"
              case "chisel3.util.log2Floor.apply" => s"log2Floor(${args})"

              case _ => s"TODO(${name}(${args}))"
            }
          case SIdent(name, tpe) =>
            s"${name.toString()}(${args})"
          case s: SApply =>
            s"${s.toCode}(${args})"
          case _ => s"TODO(sApplyCode ${sApply.fun.toCode}(${args}))"
        }

      }
      private def sSelectCode(sSelect: SSelect): String = {
        val from = sSelect.from.toCode
        val name = sSelect.name.toString()
        (name match {
          case "getWidth" => Some("width")
          case "tail"     => Some("tail")
          case "last"     => Some("last")
          case _          => None
        }).map(func => s"${from}.${func}")
          .getOrElse(
            name match {
              case "bitLength" => s"bitLength(${from})"
              case _           => s"TODO(SSelect ${sSelect})"
            }
          )
      }
      private def sTupleCode(sTuple: STuple): String = {
        s"(${sTuple.args.map(_.toCode).mkString(", ")})"
      }
      private def connectCL(connect: Connect): CodeLines = {
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
      }
      private def whenCL(
          when: When,
          isElseWhen: Boolean
      ): CodeLines = {
        val keyword = if (isElseWhen) " else if" else "if"
        val whenPart = CodeLines.warpToNewLine(
          CodeLines.warpToOneLine(s"${keyword} (when(", when.cond.toCodeLines, ")) {"),
          when.whenBody.map(_.toCodeLines).foldLeft(CodeLines.empty)(_ ++ _).indented,
          "}"
        )
        val otherPart: CodeLines = {
          if (when.hasElseWhen) {
            val elseWhen = when.otherBody.head.asInstanceOf[When]
            whenCL(elseWhen, true)
          } else {
            val other = when.otherBody.map(_.toCodeLines).foldLeft(CodeLines.empty)(_ ++ _)
            if (other.isEmpty)
              CodeLines.empty
            else
              CodeLines.warpToNewLine(
                " else {",
                other.indented,
                "}"
              )
          }
        }
        whenPart.concatLastLine(otherPart)
      }
      private def switchCL(switch: Switch): CodeLines = {
        val signal = switch.cond.toCode
        val branchs = switch.branchs.map { case (value, statments) =>
          CodeLines.warpToOneLine(
            s"if (${signal} == ${value.toCode}) {",
            statments.map(_.toCodeLines).toCodeLines.indented,
            "}"
          )
        }
        (branchs.head :: branchs.tail.map(" else ".concatLastLine(_)))
          .reduce(_.concatLastLine(_))
      }
      private def sIfCL(sIf: SIf): CodeLines = {
        val cond  = sIf.cond.toCode
        val thenp = sIf.thenp.toCodeLines
        val elsep = sIf.elsep.toCodeLines
        CodeLines
          .warpToOneLine("if (", cond, ") ")
          .concatLastLine(thenp)
          .concatLastLine({
            if (elsep.isEmpty) CodeLines.empty
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
        sFunction.body match {
          case SMatch(selector, head :: Nil, tpe) =>
            val tuple = head.tupleNames.map(_._1.toString()).mkString(", ")
            val body  = head.body.toCodeLines
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
          case _ => CodeLines(s"TODO(sFunctionCL ${sFunction})")
        }
      }
    }
  }
}
