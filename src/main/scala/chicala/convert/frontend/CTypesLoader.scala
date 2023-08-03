package chicala.convert.frontend

import scala.tools.nsc.Global

trait CTypesLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object CDirectionLoader {
    def apply(tr: Tree): Option[CDirection] = passThrough(tr)._1 match {
      /* Select(Select(Ident(chisel3), chisel3.Input), TermName("apply")) */
      case Select(Select(Ident(TermName(chisel3)), tpe), TermName("apply")) =>
        tpe match {
          case TermName("Input")   => Some(Input)
          case TermName("Output")  => Some(Output)
          case TermName("Flipped") => Some(Flipped)
          case _                   => None
        }
      case _ => None
    }
  }

  object CTypeLoader {
    def getSomeWidth(args: List[Tree]): Option[Tree] = args match {
      case Select(Apply(Select(cp, TermName("fromIntToWidth")), List(w)), TermName("W")) :: next
          if isChisel3Package(cp) =>
        Some(w)
      case _ => None
    }

    private def getVecArgs(args: List[Tree]): (Tree, CType) = {
      if (args.length == 2) {
        val size      = args.head
        val cDataType = CTypeLoader(args.tail.head)
        (size, cDataType)
      } else {
        reporter.error(args.head.pos, "Unknow arg of Vec")
        (args.head, CType.empty)
      }
    }

    def fromString(tpe: String): Option[CType] = {
      tpe match {
        case "chisel3.UInt" => Some(UInt(Node, Undirect))
        case "chisel3.SInt" => Some(SInt(Node, Undirect))
        case "chisel3.Bool" => Some(Bool(Node, Undirect))
        case _              => None
      }
    }

    def fromTypeTree(tree: Tree): CType = {
      fromString(tree.tpe.toString()) match {
        case Some(value) =>
          value
        case None =>
          reporter.error(tree.pos, "unknow data type in CTypeLoader.fromTypeTree")
          CType.empty
      }
    }

    def apply(tr: Tree): CType = {
      val tree = passThrough(tr)._1
      tree match {
        case t: TypeTree if isChiselType(t) => fromTypeTree(t)
        case Apply(fun, args) =>
          val someDirection = CDirectionLoader(fun)
          someDirection match {
            /* Apply(<Input(_)>, List(<UInt(width.W)>)) */
            case Some(direction) =>
              val cDataType = CTypeLoader(args.head)
              cDataType.updatedDriction(direction)

            /* Apply(<UInt(_)>, List(<width.W>)) */
            case _ =>
              val f = passThrough(fun)._1
              f match {
                case Select(Select(cp, tpe), TermName("apply")) if isChisel3Package(cp) =>
                  val someWidth = getSomeWidth(args)
                  tpe match {
                    case TermName("UInt") => UInt(Node, Undirect)
                    case TermName("SInt") => UInt(Node, Undirect)
                    case TermName("Bool") => Bool(Node, Undirect)
                    case TermName("Vec") =>
                      val (size, cType) = getVecArgs(args)
                      Vec(Node, cType) // size
                    case _ =>
                      unprocessedTree(f, "CTypeLoader")
                      CType.empty
                  }
                case _ =>
                  unprocessedTree(f, "CTypeLoader")
                  CType.empty
              }
          }

        case Block(stats, _) =>
          BundleDefLoader(stats.head).get.bundle
        case _ =>
          unprocessedTree(tree, "CTypeLoader")
          CType.empty
      }
    }
  }

  object STypeLoader {
    def apply(tr: Tree): SType = {
      StInt
    }
  }
}
