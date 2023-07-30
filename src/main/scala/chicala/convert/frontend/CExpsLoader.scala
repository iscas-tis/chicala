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
              val signalInfo = SignalInfo(Node, CDataTypeLoader(someTpe.get))
              CApply(op, signalInfo, (qualifier :: args).map(CExpLoader(cInfo, _)))
            case None => {
              unprocessedTree(tree, "CExpLoader")
              EmptyExp // empty
            }
          }
        case a @ Apply(fun, args) =>
          val f     = passThrough(fun)._1
          val fName = f.toString()
          COp(fName) match {
            case Some(op) =>
              val signalInfo = SignalInfo(Node, CDataTypeLoader.fromTreeTpe(a))
              CApply(op, signalInfo, args.map(CExpLoader(cInfo, _)))
            case None =>
              SExp(SApplyLoader(cInfo, a).get._2.get, SignalInfo.empty) // SApply has no SignalInfo
          }
        case s @ Select(qualifier, name) =>
          if (isChiselType(s)) {
            if (isChiselType(qualifier)) {
              COp(name.toString()) match {
                case Some(op) =>
                  CApply(op, SignalInfo(Node, CDataTypeLoader(someTpe.get)), List(CExpLoader(cInfo, qualifier)))
                case None =>
                  if (isChiselType(s))
                    SignalRef(s, cInfo.getSignalInfo(s))
                  else {
                    reporter.error(tree.pos, s"Unknow op name in CExp ${name}")
                    EmptyExp
                  }
              }
            } else if (isChiselLiteralType(qualifier)) {
              val (literal: Literal, value: BigInt, width: Int) =
                if (isChisel3FromIntToLiteralType(qualifier)) {
                  val literal = qualifier.asInstanceOf[Apply].args.head.asInstanceOf[Literal]
                  val value   = BigInt(literal.value.stringValue)
                  val width   = value.bitLength
                  (literal, value, width)
                } else if (isChisel3FromStringToLiteralType(qualifier)) {
                  val literal     = qualifier.asInstanceOf[Apply].args.head.asInstanceOf[Literal]
                  val (base, num) = literal.value.stringValue.splitAt(1)
                  val radix = base match {
                    case "x" | "h" => 16
                    case "d"       => 10
                    case "o"       => 8
                    case "b"       => 2
                    case _ =>
                      reporter.error(literal.pos, "Unknow literal base")
                      2
                  }
                  val value = BigInt(num, radix)
                  val width = value.bitLength
                  (literal, value, width)
                }

              name.toString() match {
                case "U" => Lit(literal, SignalInfo(Node, UInt(Literal(Constant(width)), Undirect)))
                case "S" => Lit(literal, SignalInfo(Node, SInt(Literal(Constant(width)), Undirect)))
                case _ =>
                  reporter.error(tree.pos, s"Unknow name in CExp ${name}")
                  EmptyExp
              }
            } else {
              SignalRef(s, cInfo.getSignalInfo(s))
            }
          } else {
            SExp(SSelect(s), SignalInfo.empty) // SSelect has no SignalInfo
          }
        case i @ Ident(name) =>
          if (isChiselType(i))
            SignalRef(i, cInfo.getSignalInfo(i))
          else {
            unprocessedTree(tree, "CExpLoader.case.Ident")
            EmptyExp
          }
        case l @ Literal(value) =>
          SExp(SLiteral(l), SignalInfo.empty) // SLiteral has no SignalInfo

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
      "do_apply"        -> Slice,
      "do_unary_$bang"  -> Not,
      "do_unary_$minus" -> UnaryMinus,
      "do_$plus"        -> Add,
      "do_$minus"       -> Minus,
      "do_$bar$bar"     -> Or,
      "do_$amp$amp"     -> And,
      "do_$up"          -> Xor,
      "do_$eq$eq$eq"    -> Equal,
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
