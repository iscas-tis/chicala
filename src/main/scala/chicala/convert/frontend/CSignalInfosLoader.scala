package chicala.convert.frontend

import scala.tools.nsc.Global

trait CSignalInfosLoader { self: Scala2Loader =>
  val global: Global
  import global._

  object CDirectionLoader {
    def apply(tree: Tree): CDirection = {
      tree match {
        /*
          TypeApply(
            Select(Select(Ident(chisel3), chisel3.Input), TermName("apply")),
            List(TypeTree())
          )
         */
        case TypeApply(Select(Select(Ident(TermName(chisel3)), tpe), TermName("apply")), _) =>
          tpe match {
            case TermName("Input")   => Input
            case TermName("Output")  => Output
            case TermName("Flipped") => Flipped
            case _ =>
              reporter.error(tree.pos, s"Unknow CDirection: ${tree}")
              Undirect
          }
        case _ =>
          reporter.error(tree.pos, s"Unknow CDirection: ${tree}")
          Undirect
      }
    }
  }

  object CDataTypeLoader {
    def apply(tree: Tree): CDataType = {
      tree match {
        /* Apply(Apply(<Input(_)>, List(<UInt(width.W)>)), List(<someCompileOptions>)) */
        case Apply(Apply(typeApply, args), _) => {
          val direction = CDirectionLoader(typeApply)
          val cDataType = CDataTypeLoader(args.head)
          cDataType.updateDriction(direction)
        }
        /* Apply(<UInt(_)>, List(<width.W>)) */
        case Apply(Select(Select(cp, tpe), TermName("apply")), args) if Chisel3Package(cp) => {
          assert(args.length <= 1, "width args at mose one")
          val someWidth = args match {
            case Select(Apply(Select(cp, TermName("fromIntToWidth")), List(w)), TermName("W")) :: next
                if Chisel3Package(cp) =>
              Some(w)
            case _ => None
          }
          tpe match {
            case TermName("UInt") => UInt(someWidth.get, Undirect)
            case TermName("SInt") => UInt(someWidth.get, Undirect)
            case TermName("Bool") => Bool(Undirect)
            case _                => Bool(Undirect)
          }
        }
      }
    }
  }
}
