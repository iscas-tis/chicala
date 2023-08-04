package chicala.convert.frontend

import scala.tools.nsc.Global

trait COpsMatcher { self: Scala2Reader =>
  val global: Global
  import global._

  object COp {
    val nameToObj: Map[String, COp] = Map(
      // CCalculOp
      "do_apply"        -> Slice,
      "do_unary_$bang"  -> Not,
      "do_unary_$minus" -> UnaryMinus,
      //
      "do_$plus"       -> Add,
      "do_$minus"      -> Minus,
      "do_$bar$bar"    -> Or,
      "do_$amp$amp"    -> And,
      "do_$up"         -> Xor,
      "do_$less$less"  -> LShift,
      "do_$eq$eq$eq"   -> Equal,
      "do_$greater$eq" -> GreaterEq,
      //
      "do_asUInt" -> AsUInt,
      // CUtilOp
      "chisel3.Mux.do_apply"    -> Mux,
      "chisel3.util.Cat.apply"  -> Cat,
      "chisel3.util.Fill.apply" -> Fill,
      "chisel3.util.Log2.apply" -> Log2
    )

    def apply(opName: String): Option[COp] = {
      if (nameToObj.contains(opName)) Some(nameToObj(opName))
      else None
    }
  }
}
