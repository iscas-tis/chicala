package chicala.convert.frontend

import scala.tools.nsc.Global

trait CExpsLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object CExpLoader {
    def apply(cInfo: CircuitInfo, tree: Tree): CExp = {
      val (subTree, someTpe) = passThrough(tree)
      subTree match {

        case Apply(Select(qualifier, name), args) => {
          val opName = name.toString()
          COp.lookup(opName) match {
            case Some(value) => {
              value match {
                case x: CUnaryOpObj =>
                  reporter.error(subTree.pos, "should not be here in CExpLoader error1")
                  NoneExp
                case x: CBinaryOpObj =>
                  val left  = apply(cInfo, qualifier)
                  val right = apply(cInfo, args.head)
                  x.gen(left, right)
                case x: CTernaryOpObj =>
                  unprocessedTree(subTree, "CExpLoader")
                  NoneExp // empty
              }
            }
            case None => {
              unprocessedTree(subTree, "CExpLoader")
              NoneExp // empty
            }
          }
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
                reporter.error(subTree.pos, s"Unknow name in CExp ${name}")
                NoneExp
            }
          } else {
            COp.lookup(name.toString()) match {
              case Some(value) =>
                value match {
                  case x: CUnaryOpObj =>
                    val inner = apply(cInfo, qualifier)
                    x.gen(inner)
                  case _ =>
                    reporter.error(subTree.pos, "should not be here in CExpLoader error2")
                    NoneExp
                }
              case None =>
                SignalRef(subTree, cInfo.getSignalInfo(subTree))
            }
          }
        }

        case _ => {
          unprocessedTree(subTree, "CExpLoader")
          NoneExp
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
  sealed abstract class CTernaryOpObj(override val chiselInnerName: String, val gen: (CExp, CExp, CExp) => CTernaryOp)
      extends COpObj(chiselInnerName)

  case object NotOp   extends CUnaryOpObj("do_unary_$bang", new Not(_))
  case object AddOp   extends CBinaryOpObj("do_$plus", new Add(_, _))
  case object OrOp    extends CBinaryOpObj("do_$bar$bar", new Or(_, _))
  case object AndOp   extends CBinaryOpObj("do_$amp$amp", new And(_, _))
  case object EqualOp extends CBinaryOpObj("do_$eq$eq$eq", new Equal(_, _))
  case object MuxOp   extends CTernaryOpObj("do_mux?", new Mux(_, _, _))

  object COp {
    type k = And
    val ops: Set[COpObj] = Set(NotOp, AddOp, OrOp, AndOp, EqualOp, MuxOp)
    val nameToObj: Map[String, COpObj] = (ops.map { x =>
      (x.chiselInnerName -> x)
    }).toMap

    def lookup(opName: String): Option[COpObj] = {
      if (nameToObj.contains(opName)) Some(nameToObj(opName))
      else None
    }
  }
}
