package chicala.convert.frontend

import scala.tools.nsc.Global

trait COpsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object COpLoader {
    val nameToObj: Map[String, COp] = Map(
      // CCalculOp
      "do_apply"        -> Slice,
      "do_unary_$bang"  -> LogiNot,
      "do_unary_$tilde" -> Not,
      "do_unary_$minus" -> Negative,
      //
      "do_$plus"  -> Add,
      "do_$minus" -> Minus,
      "do_$times" -> Multiply,
      //
      "do_$amp"             -> And,
      "do_$bar"             -> Or,
      "do_$up"              -> Xor,
      "do_$less$less"       -> LShift,
      "do_$greater$greater" -> RShift,
      //
      "do_$eq$eq$eq"   -> Equal,
      "do_$eq$div$eq"  -> NotEqual,
      "do_$greater$eq" -> GreaterEq,
      //
      "do_$amp$amp" -> LogiAnd,
      "do_$bar$bar" -> LogiOr,
      //
      "apply" -> VecSelect,
      "take"  -> VecTake,

      //
      "do_asUInt" -> AsUInt,
      "do_asSInt" -> AsSInt,

      // CUtilOp
      "chisel3.Mux.do_apply"         -> Mux,
      "chisel3.util.MuxLookup.apply" -> MuxLookup,
      "chisel3.util.Cat.apply"       -> Cat,
      "chisel3.util.Fill.apply"      -> Fill,
      "chisel3.util.Log2.apply"      -> Log2
    )

    def apply(opName: String): Option[COp] = {
      if (nameToObj.contains(opName)) Some(nameToObj(opName))
      else None
    }
  }
}
