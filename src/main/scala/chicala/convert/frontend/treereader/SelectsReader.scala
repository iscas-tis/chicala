package chicala.convert.frontend

import scala.tools.nsc.Global

trait SelectsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object SelectReader {
    val sLibs: List[String] = List(
      "scala.`package`.BigInt.apply",
      "math.this.BigInt.int2bigInt",
      "chisel3.util.log2Ceil.apply",
      "chisel3.util.log2Floor.apply",
      "chisel3.util.log2Up.apply",
      "scala.Predef.intWrapper",
      "scala.Array.fill",
      "scala.`package`.Seq.apply"
    )
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MTerm])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case s @ Select(qualifier, name: TermName) =>
          if (sLibs.contains(s.toString()))
            return Some((cInfo, Some(SLib(s.toString(), StFunc))))

          if (isChiselSignalType(tpt)) {
            if (isChiselSignalType(qualifier) || isChiselModuleType(qualifier)) {
              COpLoader(name.toString()) match {
                case Some(op) => // unary operator
                  val operands = List(MTermLoader(cInfo, qualifier).get._2.get)
                  val cApply   = CApply(op, SignalTypeLoader.fromTpt(tpt).get, operands)
                  Some((cInfo, Some(cApply)))
                case None => // select from bundle / module io / This
                  Some((cInfo, Some(SignalRef(s, cInfo.getSignalType(s)))))
              }
            } else if (isChiselLiteralType(qualifier)) {
              LitLoader(cInfo, tr)
            } else {
              val from = MTermLoader(cInfo, qualifier).get._2.get
              val tpe  = MTypeLoader.fromTpt(tpt).get
              Some((cInfo, Some(SSelect(from, name, tpe))))
            }
          } else {
            val tpe = MTypeLoader.fromTpt(tpt).get
            qualifier match {
              case This(cInfo.name) => Some((cInfo, Some(SIdent(name, tpe))))
              case Ident(innerName: TermName) =>
                val sSelect = SSelect(SIdent(innerName, MTypeLoader.fromTpt(qualifier).get), name, tpe)
                Some((cInfo, Some(sSelect)))
              case t =>
                val sSelect = SSelect(MTermLoader(cInfo, t).get._2.get, name, tpe)
                Some((cInfo, Some(sSelect)))
            }
          }

        case _ =>
          unprocessedTree(tree, "SelectReader")
          None
      }

    }

  }

}
