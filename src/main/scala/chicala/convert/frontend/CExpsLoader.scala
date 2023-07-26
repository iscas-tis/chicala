package chicala.convert.frontend

import scala.tools.nsc.Global

trait CExpsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object CExpLoader {
    def apply(cInfo: CircuitInfo, tr: Tree): CExp = {
      val (tree, someTpe) = passThrough(tr)
      tree match {

        case Apply(Select(qualifier, name), args) if isChiselType(qualifier) => {
          val opName = name.toString()
          COp.lookup(opName) match {
            case Some(value) => {
              value match {
                case x: CBinaryOpObj =>
                  val left  = apply(cInfo, qualifier)
                  val right = apply(cInfo, args.head)
                  x.gen(left, right)
                case _ =>
                  reporter.error(tree.pos, "should not be here in CExpLoader error1")
                  EmptyExp
              }
            }
            case None => {
              unprocessedTree(tree, "CExpLoader")
              EmptyExp // empty
            }
          }
        }
        case a @ Apply(fun, args) =>
          val f     = passThrough(fun)._1
          val fName = f.toString()
          COp.lookup(fName) match {
            case Some(value) =>
              value match {
                case x: CMultiOpObj =>
                  x.gen(args.map(CExpLoader(cInfo, _)))
                case _ =>
                  reporter.error(tree.pos, "should not be here in CExpLoader error3")
                  EmptyExp
              }
            case None => SExp(SApply(a))
          }

        case Select(qualifier, name) => {
          if (qualifier.tpe.toString() == "chisel3.fromIntToLiteral") {
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
          } else {
            COp.lookup(name.toString()) match {
              case Some(value) =>
                value match {
                  case x: CUnaryOpObj =>
                    val inner = CExpLoader(cInfo, qualifier)
                    x.gen(inner)
                  case _ =>
                    reporter.error(tree.pos, "should not be here in CExpLoader error2")
                    EmptyExp
                }
              case None =>
                SignalRef(tree, cInfo.getSignalInfo(tree))
            }
          }
        }
        case EmptyTree => EmptyExp

        case i @ Ident(name) =>
          if (isChiselType(i))
            SignalRef(i, cInfo.getSignalInfo(i))
          else {
            unprocessedTree(tree, "CExpLoader.case.Ident")
            EmptyExp
          }

        case _ => {
          unprocessedTree(tree, "CExpLoader")
          EmptyExp
        }

      }
    }

  }

  // operator objects
  sealed abstract class COpObj(val chiselInnerName: String)
  sealed abstract class CUnaryOpObj(override val chiselInnerName: String, val gen: (CExp) => CUnaryOp)
      extends COpObj(chiselInnerName)
  sealed abstract class CBinaryOpObj(override val chiselInnerName: String, val gen: (CExp, CExp) => CBinaryOp)
      extends COpObj(chiselInnerName)
  sealed abstract class CMultiOpObj(override val chiselInnerName: String, val gen: (List[CExp]) => CMultiOp)
      extends COpObj(chiselInnerName)

  case object NotOp extends CUnaryOpObj("do_unary_$bang", new Not(_))

  case object AddOp   extends CBinaryOpObj("do_$plus", new Add(_, _))
  case object OrOp    extends CBinaryOpObj("do_$bar$bar", new Or(_, _))
  case object AndOp   extends CBinaryOpObj("do_$amp$amp", new And(_, _))
  case object EqualOp extends CBinaryOpObj("do_$eq$eq$eq", new Equal(_, _))

  case object CatOp  extends CMultiOpObj("chisel3.util.Cat.apply", new Cat(_))
  case object FillOp extends CMultiOpObj("chisel3.util.Fill.apply", new Cat(_))

  object COp {
    val ops: Set[COpObj] = Set(NotOp) ++
      Set(AddOp, OrOp, AndOp, EqualOp) ++
      Set(CatOp, FillOp)
    val nameToObj: Map[String, COpObj] = (ops.map { x =>
      (x.chiselInnerName -> x)
    }).toMap

    def lookup(opName: String): Option[COpObj] = {
      if (nameToObj.contains(opName)) Some(nameToObj(opName))
      else None
    }
  }
}
