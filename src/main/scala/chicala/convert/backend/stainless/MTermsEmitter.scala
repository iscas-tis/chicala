package chicala.convert.backend.stainless

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.convert.backend.util._

trait MTermsEmitter { self: StainlessEmitter with ChicalaAst =>
  val global: Global
  import global._

  trait MTermEmitterImplicit { self: StainlessEmitterImplicit =>
    implicit class MTermEmitter(mTerm: MTerm) {
      def toCode: String = mTerm match {
        case s: SignalRef       => signalRefCode(s)
        case c: Connect         => connectCode(c)
        case c: CApply          => cApplyCode(c)
        case l: Lit             => litCode(l)
        case a: Assert          => assertCode(a)
        case s: SApply          => sApplyCode(s)
        case s: SSelect         => sSelectCode(s)
        case SIdent(name, _)    => name.toString()
        case SLiteral(value, _) => value.toString()
        case _                  => s"TODO(Code ${mTerm})"
      }
      def toCodeLines: CodeLines = mTerm match {
        case _: SignalRef | _: Connect | _: CApply | _: Assert => mTerm.toCode

        case w: When => whenCL(w, false)
        case _       => CodeLines(s"TODO(CL ${mTerm})")
      }

      private def signalRefCode(signalRef: SignalRef): String = {
        def getNameFromTree(tree: Tree): String = {
          tree match {
            case Ident(name)             => name.toString()
            case Select(This(_), name)   => name.toString()
            case Select(qualifier, name) => s"${getNameFromTree(qualifier)}_${name}"
            case _                       => s"TODO(signalRefCode ${tree})"
          }
        }
        getNameFromTree(signalRef.name)
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
      private def connectCode(connect: Connect): String = {
        val left = connect.left.toCode
        val expr = connect.expr.toCode
        s"${left} = ${left} := ${expr}"
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
          case u: CUtilOp => s"${op}(${operands.mkString(", ")})"
          case _          => s"TODO(${cApply})"
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
          case SSelect(from, name, tpe) =>
            name.toString match {
              case "$minus" => s"(${from.toCode} - ${args})"
              case "$div"   => s"(${from.toCode} / ${args})"
              case s        => s"(${from.toCode} TODO(sApplyCode ${s}) ${args})"
            }
          case x => s"${x.toCode}(${args})"
        }

      }
      private def sSelectCode(sSelect: SSelect): String = {
        s"TODO(SSelect ${sSelect})"
      }
    }
  }
}
