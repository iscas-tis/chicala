package chicala.convert.frontend

import scala.tools.nsc.Global

trait CExpsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object CExpLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): MTerm = {
      val (tree, tpt) = passThrough(tr)

      tree match {
        case Apply(Select(qualifier, name), args) if isChiselType(qualifier) =>
          val opName = name.toString()
          COp(opName) match {
            case Some(op) =>
              val signalInfo = CTypeLoader(tpt)
              CApply(op, signalInfo, (qualifier :: args).map(CExpLoader(cInfo, _)))
            case None => {
              unprocessedTree(tree, "CExpLoader")
              EmptyMTerm // empty
            }
          }
        case Apply(Select(qualifier, name), args) if isChiselLiteralType(qualifier) => {
          // 0.U(1.W)
          val litTree = qualifier.asInstanceOf[Apply].args.head
          val litExp  = CExpLoader(cInfo, litTree).asInstanceOf[STerm]
          val width   = CTypeLoader.getSomeWidth(args).get

          name.toString() match {
            case "U" => Lit(litExp, UInt(Node, Undirect)) // width
            case "S" => Lit(litExp, SInt(Node, Undirect)) // width
            case _ =>
              reporter.error(tree.pos, s"Unknow name in CExp ${name}")
              EmptyMTerm
          }
        }
        case a @ Apply(fun, args) =>
          val f     = passThrough(fun)._1
          val fName = f.toString()
          COp(fName) match {
            case Some(op) =>
              val signalInfo = CTypeLoader.fromTypeTree(tpt)
              CApply(op, signalInfo, args.map(CExpLoader(cInfo, _)))
            case None =>
              SApplyLoader(cInfo, a).get._2.get // SApply has no SignalInfo
          }
        case s @ Select(qualifier, name) =>
          if (isChiselType(tpt)) {
            if (isChiselType(qualifier)) {
              COp(name.toString()) match {
                case Some(op) =>
                  CApply(op, CTypeLoader(tpt), List(CExpLoader(cInfo, qualifier)))
                case None =>
                  if (isChiselType(s))
                    SignalRef(s, cInfo.getCType(s))
                  else {
                    reporter.error(tree.pos, s"Unknow op name in CExp ${name}")
                    EmptyMTerm
                  }
              }
            } else if (isChiselLiteralType(qualifier)) {
              val litTree = qualifier.asInstanceOf[Apply].args.head
              val litExp  = CExpLoader(cInfo, litTree).asInstanceOf[STerm]

              name.toString() match {
                case "U" => Lit(litExp, UInt(Node, Undirect))
                case "S" => Lit(litExp, SInt(Node, Undirect))
                case _ =>
                  reporter.error(tree.pos, s"Unknow name in CExp ${name}")
                  EmptyMTerm
              }
            } else {
              SignalRef(s, cInfo.getCType(s))
            }
          } else {
            SSelect(s, EmptyMType) // SSelect has no SignalInfo
          }
        case i @ Ident(name) =>
          if (isChiselType(i))
            SignalRef(i, cInfo.getCType(i))
          else {
            unprocessedTree(tree, "CExpLoader.case.Ident")
            EmptyMTerm
          }
        case l @ Literal(value) =>
          SLiteral(l, EmptyMType) // SLiteral has no SignalInfo

        case EmptyTree => EmptyMTerm
        case _ => {
          unprocessedTree(tree, "CExpLoader")
          EmptyMTerm
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
