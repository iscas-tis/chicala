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
        case w: WireDef => wireDefCode(w)
        case _: IoDef   => s"DONOTCALLME(Code ${mDef})"
        case _          => s"TODO(Code ${mDef})"
      }

      def toCodeLines: CodeLines = mDef match {
        case IoDef(name, tpe) => CodeLines.empty
        case _: WireDef       => mDef.toCode
        case s: SDefDef       => sDefDefCL(s)
        case _                => CodeLines(s"TODO(CL ${mDef})")
      }

      private def wireDefCode(wireDef: WireDef): String = {
        val name = wireDef.name.toString()
        val init = wireDef.someInit
          .map(_.toCode)
          .getOrElse(wireDef.tpe.toCode_empty)

        s"var ${name} = ${init}"
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
