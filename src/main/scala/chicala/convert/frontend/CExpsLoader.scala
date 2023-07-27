package chicala.convert.frontend

import scala.tools.nsc.Global

trait CExpsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object CExpLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): CExp = {
      val (tree, someTpe) = passThrough(tr)
      tree match {

        case Apply(Select(qualifier, name), args) if isChiselType(qualifier) =>
          val opName = name.toString()
          COp(opName) match {
            case Some(op) =>
              CApply(op, (qualifier :: args).map(CExpLoader(cInfo, _)))
            case None => {
              unprocessedTree(tree, "CExpLoader")
              EmptyExp // empty
            }
          }
        case a @ Apply(fun, args) =>
          val f     = passThrough(fun)._1
          val fName = f.toString()
          COp(fName) match {
            case Some(op) => CApply(op, args.map(CExpLoader(cInfo, _)))
            case None     => SExp(SApplyLoader(cInfo, a).get._2.get)
          }
        case s @ Select(qualifier, name) if qualifier.tpe.toString() == "chisel3.fromIntToLiteral" =>
          val literal = qualifier.asInstanceOf[Apply].args.head.asInstanceOf[Literal]
          val value   = BigInt(literal.value.stringValue)
          val width   = value.bitLength
          name.toString() match {
            case "U" => Lit(literal, SignalInfo(Node, UInt(Literal(Constant(width)), Undirect)))
            case "S" => Lit(literal, SignalInfo(Node, SInt(Literal(Constant(width)), Undirect)))
            case _ =>
              reporter.error(tree.pos, s"Unknow name in CExp ${name}")
              EmptyExp
          }
        case s @ Select(qualifier, name) =>
          if (isChiselType(qualifier)) {
            COp(name.toString()) match {
              case Some(op) =>
                CApply(op, List(CExpLoader(cInfo, qualifier)))
              case None =>
                if (isChiselType(s))
                  SignalRef(s, cInfo.getSignalInfo(s))
                else {
                  reporter.error(tree.pos, s"Unknow op name in CExp ${name}")
                  EmptyExp
                }
            }
          } else if (isChiselType(s)) {
            SignalRef(s, cInfo.getSignalInfo(s))
          } else {
            SExp(SSelect(s))
          }
        case i @ Ident(name) =>
          if (isChiselType(i))
            SignalRef(i, cInfo.getSignalInfo(i))
          else {
            unprocessedTree(tree, "CExpLoader.case.Ident")
            EmptyExp
          }
        case l @ Literal(value) =>
          SExp(SLiteral(l))

        case EmptyTree => EmptyExp
        case _ => {
          unprocessedTree(tree, "CExpLoader")
          EmptyExp
        }

      }
    }

  }

  object COp {
    val nameToObj: Map[String, COp] = Map(
      // CCalculOp
      "do_apply"       -> Slice,
      "do_unary_$bang" -> Not,
      "do_$plus"       -> Add,
      "do_$bar$bar"    -> Or,
      "do_$amp$amp"    -> And,
      "do_$eq$eq$eq"   -> Equal,
      // CUtilOp
      "chisel3.util.Cat.apply"  -> Cat,
      "chisel3.util.Fill.apply" -> Fill
    )

    def apply(opName: String): Option[COp] = {
      if (nameToObj.contains(opName)) Some(nameToObj(opName))
      else None
    }
  }
}
