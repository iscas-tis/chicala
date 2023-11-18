package chicala.convert.backend.stainless

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.convert.backend.util._
import chicala.ChicalaConfig

trait MTypesEmitter { self: StainlessEmitter with ChicalaAst =>
  val global: Global
  import global._

  trait MTypeEmitterImplicit { self: StainlessEmitterImplicit =>
    implicit class MTypeEmitter(tpe: MType) {
      def toCode: String = tpe match {
        case cType: CType =>
          cType match {
            case _: UInt => "UInt"
            case _: SInt => "SInt"
            case _: Bool => "Bool"
            case Vec(_, _, tparam) =>
              if (ChicalaConfig.simulation) s"Seq[${tparam.toCode}]"
              else s"List[${tparam.toCode}]"
            case x => TODO(s"$x")
          }
        case sType: SType =>
          sType match {
            case StInt            => if (ChicalaConfig.simulation) "Int" else "BigInt"
            case StBigInt         => "BigInt"
            case StBoolean        => "Boolean"
            case StTuple(tparams) => s"(${tparams.map(_.toCode).mkString(", ")})"
            case StSeq(tparam) =>
              if (ChicalaConfig.simulation) s"Seq[${tparam.toCode}]"
              else s"List[${tparam.toCode}]"
            case StArray(tparam) =>
              if (ChicalaConfig.simulation) s"Seq[${tparam.toCode}]"
              else s"List[${tparam.toCode}]"
            case StUnit => "Unit"

            case StWrapped("Nothing") => "Nothing"
            case x                    => TODO(s"SType $x")
          }
        case EmptyMType => TODO("EmptyMType")
      }
    }
    implicit class SignalTypeEmitter(tpe: SignalType) {
      def toCode_init(name: String): String = s"var ${name} = ${toCode_empty}"
      def toCode_empty: String = tpe match {
        case Bool(physical, direction)                   => s"${tpe.toCode}.empty()"
        case UInt(width: KnownSize, physical, direction) => s"${tpe.toCode}.empty(${width.width.toCode})"
        case SInt(width: KnownSize, physical, direction) => s"${tpe.toCode}.empty(${width.width.toCode})"
        case Vec(size: KnownSize, physical, tparam) =>
          if (ChicalaConfig.simulation)
            s"Seq.fill(${size.width.toCode})(${tparam.toCode_empty})"
          else
            s"List.fill(${size.width.toCode})(${tparam.toCode_empty})"
        case _ => TODO(s"$tpe")
      }
      def toCode_regNextInit(name: String): String = s"var ${name}_next = regs.${name}"
    }
  }
}
