package chicala.ast

import scala.tools.nsc.Global

trait CExps { self: ChicalaAst =>
  val global: Global
  import global._

  sealed abstract class CExp {
    def signals: Set[String]
  }
  object CExp {
    def fromTree(cInfo: CircuitInfo, tree: Tree): CExp = {
      val (subTree, someTpe) = passThrough(tree)
      subTree match {

        case Apply(Select(qualifier, name), args) => {
          val opName = name.toString()
          COp.lookup(opName) match {
            case Some(value) => {
              value match {
                case x: CUnaryOpObj =>
                  reporter.error(subTree.pos, "should not be here in CExp.fromTree error1")
                  NoneExp
                case x: CBinaryOpObj =>
                  val left  = fromTree(cInfo, qualifier)
                  val right = fromTree(cInfo, args.head)
                  x.gen(left, right)
                case x: CTernaryOpObj =>
                  unprocessedTree(subTree, "CExp.fromTree")
                  NoneExp // empty
              }
            }
            case None => {
              unprocessedTree(subTree, "CExp.fromTree")
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
                    val inner = fromTree(cInfo, qualifier)
                    x.gen(inner)
                  case _ =>
                    reporter.error(subTree.pos, "should not be here in CExp.fromTree error2")
                    NoneExp
                }
              case None =>
                SignalRef(subTree, cInfo.getSignalInfo(subTree))
            }
          }
        }

        case _ => {
          unprocessedTree(subTree, "CExp.fromTree")
          NoneExp
        }

      }
    }

    def empty = NoneExp
  }

  case class Lit(literal: Literal, info: SignalInfo) extends CExp {
    def signals: Set[String] = Set.empty
  }
  case class SignalRef(name: Tree, info: SignalInfo) extends CExp {
    def signals: Set[String] = Set(name.toString())
  }
  case object NoneExp extends CExp {
    def signals: Set[String] = Set.empty
  }

  // operators
  sealed abstract class COp extends CExp
  sealed abstract class CUnaryOp(val inner: CExp) extends COp {
    def signals: Set[String] = inner.signals
  }
  sealed abstract class CBinaryOp(val left: CExp, val right: CExp) extends COp {
    def signals: Set[String] = left.signals ++ right.signals
  }
  sealed abstract class CTernaryOp(val a: CExp, val b: CExp, val c: CExp) extends COp {
    def signals: Set[String] = a.signals ++ b.signals ++ c.signals
  }

  case class Not(override val inner: CExp)                            extends CUnaryOp(inner)
  case class Add(override val left: CExp, override val right: CExp)   extends CBinaryOp(left, right)
  case class Or(override val left: CExp, override val right: CExp)    extends CBinaryOp(left, right)
  case class And(override val left: CExp, override val right: CExp)   extends CBinaryOp(left, right)
  case class Equal(override val left: CExp, override val right: CExp) extends CBinaryOp(left, right)
  case class Mux(condition: CExp, ifTrue: CExp, ifFalse: CExp)        extends CTernaryOp(condition, ifTrue, ifFalse)

  // operator objects
  sealed abstract class COpObj(val chiselInnerName: String)
  sealed abstract class CUnaryOpObj(override val chiselInnerName: String, val gen: (CExp) => CUnaryOp)
      extends COpObj(chiselInnerName)
  sealed abstract class CBinaryOpObj(override val chiselInnerName: String, val gen: (CExp, CExp) => CBinaryOp)
      extends COpObj(chiselInnerName)
  sealed abstract class CTernaryOpObj(override val chiselInnerName: String, val gen: (CExp, CExp, CExp) => CTernaryOp)
      extends COpObj(chiselInnerName)

  case object Not   extends CUnaryOpObj("do_unary_$bang", new Not(_))
  case object Add   extends CBinaryOpObj("do_$plus", new Add(_, _))
  case object Or    extends CBinaryOpObj("do_$bar$bar", new Or(_, _))
  case object And   extends CBinaryOpObj("do_$amp$amp", new And(_, _))
  case object Equal extends CBinaryOpObj("do_$eq$eq$eq", new Equal(_, _))
  case object Mux   extends CTernaryOpObj("do_mux?", new Mux(_, _, _))

  object COp {
    type k = And
    val ops: Set[COpObj] = Set(Not, Add, Or, And, Equal, Mux)
    val nameToObj: Map[String, COpObj] = (ops.map { x =>
      (x.chiselInnerName -> x)
    }).toMap

    def lookup(opName: String): Option[COpObj] = {
      if (nameToObj.contains(opName)) Some(nameToObj(opName))
      else None
    }
  }
}
