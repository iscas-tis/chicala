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
        case _: IoDef => s"DONOTCALLME(Code ${mDef})"
        case _        => s"TODO(Code ${mDef})"
      }

      def toCodeLines: CodeLines = mDef match {
        case _: IoDef | _: RegDef => CodeLines.empty
        case w: WireDef           => wireDefCL(w)
        case n: NodeDef           => nodeDefCL(n)
        case e: EnumDef           => enumDefCL(e)
        case s: SubModuleDef      => subModuleDefCL(s)
        case s: SValDef           => sValDefCL(s)
        case s: SUnapplyDef       => sUnapplyDefCL(s)
        case s: SDefDef           => sDefDefCL(s)
        case _                    => CodeLines(s"TODO(CL ${mDef})")
      }

      private def wireDefCL(wireDef: WireDef): CodeLines = {
        val name = wireDef.name.toString()
        val init = wireDef.someInit
          .map(_.toCodeLines)
          .getOrElse(CodeLines(wireDef.tpe.toCode_empty))

        CodeLines(s"var ${name} = ").concatLastLine(init)
      }
      private def nodeDefCL(nodeDef: NodeDef): CodeLines = {
        val valW = if (nodeDef.isVar) "var" else "val"
        val name = nodeDef.name.toString()
        val rhs  = nodeDef.rhs.toCodeLines
        CodeLines(s"${valW} ${name} = ").concatLastLine(rhs)
      }
      private def enumDefCL(enumDef: EnumDef): CodeLines = {
        val left  = enumDef.names.map(_.toString()).mkString(", ")
        val width = enumDef.tpe.width.asInstanceOf[KnownSize].width.toCode
        val right = (0 until enumDef.names.size)
          .map(i => s"Lit(${i}, ${width}).U")
          .mkString(", ")
        CodeLines(s"val (${left}) = (${right})")
      }
      private def subModuleDefCL(subModuleDef: SubModuleDef): CodeLines = {
        val name       = subModuleDef.name.toString()
        val moduleName = subModuleDef.tpe.fullName

        CodeLines(s"val ${name} = ${moduleName}()")
      }
      private def sValDefCL(sValDef: SValDef): CodeLines = {
        val name = sValDef.name.toString()
        val rhs  = sValDef.rhs.toCodeLines
        val keyword =
          if (sValDef.isVar || sValDef.tpe.isInstanceOf[StArray]) "var"
          else "val"
        if (rhs.toCode.contains("Nothing"))
          CodeLines(s"${keyword} ${name}: ${sValDef.tpe.toCode} = ").concatLastLine(rhs)
        else
          CodeLines(s"${keyword} ${name} = ").concatLastLine(rhs)
      }
      private def sUnapplyDefCL(sUnapplyDef: SUnapplyDef): CodeLines = {
        val left  = sUnapplyDef.names.map(_.toString()).mkString(", ")
        val right = sUnapplyDef.rhs.toCodeLines
        CodeLines(s"val (${left}) = ").concatLastLine(right)
      }
      private def sDefDefCL(sDefDef: SDefDef): CodeLines = {
        val funcName: String = sDefDef.name.toString()
        val params: String = sDefDef.vparamss.map { vparams =>
          val vps = vparams.map(x => x.toCode_param).mkString(", ")
          s"(${vps})"
        }.mkString
        val returnType: String = sDefDef.tpe.toCode
        val body: CodeLines    = sDefDef.defp.toCodeLines
        CodeLines(s"def ${funcName}${params}: ${returnType} = ")
          .concatLastLine(body)
      }
    }
    implicit class MValDefEmitter(mValDef: MValDef) {
      def toCode_param: String = {
        val name = mValDef.name.toString()
        val tpe  = mValDef.tpe.toCode
        val someInit = (mValDef match {
          case SValDef(_, _, rhs, _) => rhs
          case NodeDef(_, _, rhs, _) => rhs
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
