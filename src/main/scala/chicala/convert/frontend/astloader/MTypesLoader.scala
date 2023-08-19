package chicala.convert.frontend

import scala.tools.nsc.Global

trait MTypesLoader { self: Scala2Reader =>
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

  trait MTypeLoaderLib {
    def autoTypeErasure(tr: Tree): Type = {
      if (tr.tpe.toString().endsWith(".type")) tr.tpe.erasure else tr.tpe
    }
  }

  object CTypeLoader extends MTypeLoaderLib {
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
        case "chisel3.Data" => Some(UInt.empty)
        case _              => None
      }
    }

    def fromTpt(tree: Tree): Option[CType] = {
      val tpe       = autoTypeErasure(tree)
      val someCType = fromString(tpe.toString())
      if (someCType.isEmpty)
        reporter.error(tree.pos, s"unknow data type `${tpe}` in CTypeLoader.fromTpt")
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

  object STypeLoader extends MTypeLoaderLib {

    private val wrappedTypes = List(
      "scala.collection.immutable.Range",
      "scala.collection.WithFilter[Any,[_]Any]"
    )
    private def isSeq(tpe: Type): Boolean = {
      val typeStr = tpe.toString()
      List(
        """IndexedSeq\[.*\]""",
        """Array\[.*\]"""
      ).exists(_.r.matches(typeStr))
    }

    def fromTpt(tr: Tree): Option[SType] = {
      val tpe = autoTypeErasure(tr)
      if (isScala2TupleType(TypeTree(tpe))) {
        Some(StTuple(tr.tpe.typeArgs.map(x => MTypeLoader.fromTpt(TypeTree(x)).get)))
      } else if ("""(.*): .*""".r.matches(tpe.toString())) {
        Some(StFunc)
      } else if (tr.toString() == "Any") {
        Some(StAny)
      } else if (isSeq(tpe)) {
        val tparam = MTypeLoader.fromTpt(TypeTree(tpe.typeArgs.head)).get
        Some(StSeq(tparam))
      } else {
        tpe.erasure.toString() match {
          case "Int"                     => Some(StInt)
          case "String"                  => Some(StString)
          case "scala.math.BigInt"       => Some(StBigInt)
          case "Boolean"                 => Some(StBoolean)
          case "scala.runtime.BoxedUnit" => Some(StUnit)
          case s =>
            if (!wrappedTypes.contains(s)) {
              reporter.warning(tr.pos, s"this type `${s}` need check")
            }
            Some(StWrapped(s))

        }
      }
    }
  }

  object MTypeLoader {
    def fromTpt(tr: Tree): Option[MType] = {
      if (isChiselType(tr)) CTypeLoader.fromTpt(tr)
      else STypeLoader.fromTpt(tr)
    }
    def apply(cInfo: CircuitInfo, tr: Tree): Option[MType] = {
      if (isChiselType(tr))
        CTypeLoader(cInfo, tr)
      else STypeLoader.fromTpt(tr)
    }
  }
}
