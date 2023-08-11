package chicala.convert.frontend

import scala.tools.nsc.Global

trait CTypesLoader { self: Scala2Reader =>
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
    def getWidth(cInfo: CircuitInfo, args: List[Tree]): CSize = args match {
      case Select(Apply(Select(cp, TermName("fromIntToWidth")), List(w)), TermName("W")) :: next
          if isChisel3Package(cp) =>
        KnownSize(STermLoader(cInfo, w).get._2.get)
      case _ => UnknownSize
    }

    private def getVecArgs(cInfo: CircuitInfo, args: List[Tree]): (CSize, CType) = {
      if (args.length == 2) {
        val size      = KnownSize(STermLoader(cInfo, args.head).get._2.get)
        val cDataType = CTypeLoader(cInfo, args.tail.head).get
        (size, cDataType)
      } else {
        reporter.error(args.head.pos, "Unknow arg of Vec")
        (UnknownSize, CType.empty)
      }
    }

    def fromString(tpe: String): Option[CType] = {
      tpe match {
        case "chisel3.UInt" => Some(UInt.empty)
        case "chisel3.SInt" => Some(SInt.empty)
        case "chisel3.Bool" => Some(Bool.empty)
        case _              => None
      }
    }

    def apply(tree: TypeTree): Option[CType] = {
      val someCType = fromString(tree.tpe.toString())
      if (someCType.isEmpty)
        reporter.error(tree.pos, "unknow data type in CTypeLoader.apply(tree)")
      someCType
    }

    def apply(cInfo: CircuitInfo, tr: Tree): Option[CType] = {
      val tree = passThrough(tr)._1
      tree match {
        case Apply(fun, args) =>
          val someDirection = CDirectionLoader(fun)
          someDirection match {
            /* Apply(<Input(_)>, List(<UInt(width.W)>)) */
            case Some(direction) =>
              val tpe = CTypeLoader(cInfo, args.head)
              tpe.map(_.updatedDriction(direction))

            /* Apply(<UInt(_)>, List(<width.W>)) */
            /* Apply(<new SomeBundle(_)>, List(<args>)) */
            case None =>
              val f = passThrough(fun)._1
              f match {
                case Select(Select(cp, name), TermName("apply")) if isChisel3Package(cp) =>
                  val width = getWidth(cInfo, args)
                  name match {
                    case TermName("UInt") => Some(UInt.empty.updatedWidth(width))
                    case TermName("SInt") => Some(SInt.empty.updatedWidth(width))
                    case TermName("Bool") => Some(Bool.empty)
                    case TermName("Vec") =>
                      val (size, cType) = getVecArgs(cInfo, args)
                      Some(Vec(size, Node, cType))
                    case _ =>
                      unprocessedTree(f, "CTypeLoader #1")
                      Some(CType.empty)
                  }
                case Select(New(tpt), termNames.CONSTRUCTOR) =>
                  val className     = tpt.toString()
                  val someBundleDef = cInfo.readerInfo.bundleDefs.get(className)
                  someBundleDef.map(_.bundle)
                case _ =>
                  unprocessedTree(f, "CTypeLoader #2")
                  Some(CType.empty)
              }
          }

        case Block(stats, _) =>
          Some(BundleDefLoader(cInfo, stats.head).get._2.get.bundle)
        case _ =>
          errorTree(tree, "CTypeLoader #3")
          Some(CType.empty)
      }
    }
  }

  object STypeLoader {

    def apply(tr: Tree): Option[SType] = {
      if (isScala2TupleType(tr)) {
        Some(StTuple(tr.tpe.typeArgs.map(x => MTypeLoader(TypeTree(x)).get)))
      } else if ("""(.*): .*""".r.matches(tr.tpe.toString())) {
        Some(StFunc)
      } else {
        Some(tr.tpe.erasure.toString() match {
          case "Int"               => StInt
          case "String"            => StString
          case "scala.math.BigInt" => StBigInt
          case "Boolean"           => StBoolean
          case _ =>
            errorTree(tr, s"Unknow type `${tr.tpe.erasure}`")
            StInt
        })
      }
    }
  }

  object MTypeLoader {
    def apply(tr: TypeTree): Option[MType] = {
      if (isChiselType(tr)) CTypeLoader(tr)
      else STypeLoader(tr)
    }
    def apply(cInfo: CircuitInfo, tr: Tree): Option[MType] = {
      if (isChiselType(tr))
        CTypeLoader(cInfo, tr)
      else STypeLoader(tr)
    }
  }
}
