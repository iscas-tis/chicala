package chicala.convert.backend.stainless

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.convert.backend.util._

trait MTypesEmitter { self: StainlessEmitter with ChicalaAst =>
  val global: Global
  import global._

  trait MTypeEmitterImplicit { self: StainlessEmitterImplicit =>
    implicit class MTypeEmitter(tpe: MType) {
      def toCode: String = tpe match {
        case cType: CType =>
          cType match {
            case _: UInt           => "UInt"
            case _: SInt           => "SInt"
            case _: Bool           => "Bool"
            case Vec(_, _, tparam) => s"List[${tparam.toCode}]"
            case x                 => s"TODO($x)"
          }
        case sType: SType =>
          sType match {
            case StInt            => "BigInt"
            case StBigInt         => "BigInt"
            case StBoolean        => "Boolean"
            case StTuple(tparams) => s"(${tparams.map(_.toCode).mkString(", ")})"
            case StSeq(tparam)    => s"List[${tparam.toCode}]"
            case x                => s"TODO(SType $x)"
          }
        case EmptyMType => s"TODO(EmptyMType)"
      }
    }
    implicit class SignalTypeEmitter(tpe: SignalType) {
      def toCode_init(name: String): String = s"var ${name} = ${toCode_empty}"
      def toCode_empty: String = tpe match {
        case Bool(physical, direction)                   => s"${tpe.toCode}.empty()"
        case UInt(width: KnownSize, physical, direction) => s"${tpe.toCode}.empty(${width.width.toCode})"
        case SInt(width: KnownSize, physical, direction) => s"${tpe.toCode}.empty(${width.width.toCode})"
        case Vec(size: KnownSize, physical, tparam)      => s"List.fill(${size.width.toCode})(${tparam.toCode_empty})"
        case _                                           => s"TODO($tpe)"
      }
      def toCode_regNextInit(name: String): String = s"var ${name}_next = regs.${name}"
    }
  }
}
