package chicala.convert.backend.stainless

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.convert.backend.util._

trait MDefsEmitter { self: StainlessEmitter with ChicalaAst =>
  val global: Global
  import global._

  trait MDefEmitterImplicit { self: StainlessEmitterImplicit =>
    implicit class MDefEmitter(mDef: MDef) {
      def toCode: String = mDef match {
        case w: WireDef     => wireDefCode(w)
        case n: NodeDef     => nodeDefCode(n)
        case e: EnumDef     => enumDefCode(e)
        case s: SUnapplyDef => sUnapplyDefCode(s)
        case _: IoDef       => s"DONOTCALLME(Code ${mDef})"
        case _              => s"TODO(Code ${mDef})"
      }

      def toCodeLines: CodeLines = mDef match {
        case _: IoDef | _: RegDef => CodeLines.empty
        case _: WireDef | _: NodeDef | _: EnumDef | _: SUnapplyDef =>
          mDef.toCode

        case s: SDefDef => sDefDefCL(s)
        case _          => CodeLines(s"TODO(CL ${mDef})")
      }

      private def wireDefCode(wireDef: WireDef): String = {
        val name = wireDef.name.toString()
        val init = wireDef.someInit
          .map(_.toCode)
          .getOrElse(wireDef.tpe.toCode_empty)

        s"var ${name} = ${init}"
      }
      private def nodeDefCode(nodeDef: NodeDef): String = {
        val name = nodeDef.name.toString()
        val rhs  = nodeDef.rhs.toCode
        s"val ${name} = ${rhs}"
      }
      private def enumDefCode(enumDef: EnumDef): String = {
        val left  = enumDef.names.map(_.toString()).mkString(", ")
        val width = enumDef.tpe.width.asInstanceOf[KnownSize].width.toCode
        val right = (0 until enumDef.names.size).map(i => s"Lit(${i}, ${width}).U").mkString(", ")
        s"val (${left}) = (${right})"
      }
      private def sUnapplyDefCode(sUnapplyDef: SUnapplyDef): String = {
        val left  = sUnapplyDef.names.map(_.toString()).mkString(", ")
        val right = sUnapplyDef.rhs.toCode
        s"val (${left}) = ${right}"
      }
      private def sDefDefCL(sDefDef: SDefDef): CodeLines = {
        val funcName: String = sDefDef.name.toString()
        val params: String = sDefDef.vparamss.map { vparams =>
          val vps = vparams.map(x => x.toCode_param).mkString(", ")
          s"(${vps})"
        }.mkString
        val returnType: String = sDefDef.tpe.toCode
        val body: CodeLines    = sDefDef.body.body.map(_.toCodeLines).reduce(_ ++ _)
        CodeLines(
          s"def ${funcName}${params}: ${returnType} = {",
          body.indented,
          "}"
        )
      }
    }
    implicit class MValDefEmitter(mValDef: MValDef) {
      def toCode_param: String = {
        val name = mValDef.name.toString()
        val tpe  = mValDef.tpe.toCode
        val someInit = (mValDef match {
          case SValDef(_, _, rhs, _) => rhs
          case NodeDef(_, _, rhs)    => rhs
          case _                     => EmptyMTerm
        }) match {
          case EmptyMTerm => ""
          case x          => s" = ${x.toCode}"
        }
        s"${name}: ${tpe}${someInit}"
      }
    }
  }
}
