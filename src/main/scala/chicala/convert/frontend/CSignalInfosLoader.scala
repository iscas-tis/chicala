package chicala.convert.frontend

import scala.tools.nsc.Global

trait CSignalInfosLoader { self: Scala2Loader =>
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

  object CDataTypeLoader {
    private def getSomeWidth(args: List[Tree]): Option[Tree] = args match {
      case Select(Apply(Select(cp, TermName("fromIntToWidth")), List(w)), TermName("W")) :: next
          if isChisel3Package(cp) =>
        Some(w)
      case _ => None
    }

    private def getVecArgs(args: List[Tree]): (Tree, CDataType) = {
      if (args.length == 2) {
        val size      = args.head
        val cDataType = CDataTypeLoader(args.tail.head)
        (size, cDataType)
      } else {
        reporter.error(args.head.pos, "Unknow arg of Vec")
        (args.head, Bool(Undirect))
      }
    }

    def apply(tr: Tree): CDataType = {
      val tree = passThrough(tr)._1
      tree match {
        case Apply(fun, args) =>
          val someDirection = CDirectionLoader(fun)
          someDirection match {
            /* Apply(<Input(_)>, List(<UInt(width.W)>)) */
            case Some(direction) =>
              val cDataType = CDataTypeLoader(args.head)
              cDataType.updateDriction(direction)

            /* Apply(<UInt(_)>, List(<width.W>)) */
            case _ =>
              val f = passThrough(fun)._1
              f match {
                case Select(Select(cp, tpe), TermName("apply")) if isChisel3Package(cp) =>
                  val someWidth = getSomeWidth(args)
                  tpe match {
                    case TermName("UInt") => UInt(someWidth.get, Undirect)
                    case TermName("SInt") => UInt(someWidth.get, Undirect)
                    case TermName("Bool") => Bool(Undirect)
                    case TermName("Vec") =>
                      val (size, cDataType) = getVecArgs(args)
                      Vec(size, cDataType)
                    case _ =>
                      unprocessedTree(f, "CDataTypeLoader")
                      Bool(Undirect)
                  }
                case _ =>
                  unprocessedTree(f, "CDataTypeLoader")
                  Bool(Undirect)
              }
          }

        case Block(stats, _) =>
          BundleDefLoader(stats.head).get.bundle
        case _ =>
          unprocessedTree(tree, "CDataTypeLoader")
          Bool(Undirect)
      }
    }
  }
}
