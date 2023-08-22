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

  trait MTypeLoaderLib {}

  object SignalTypeLoader extends MTypeLoaderLib {
    def getWidth(cInfo: CircuitInfo, args: List[Tree]): CSize = args match {
      case Select(Apply(Select(cp, TermName("fromIntToWidth")), List(w)), TermName("W")) :: next
          if isChisel3Package(cp) =>
        KnownSize(STermLoader(cInfo, w).get._2.get)
      case _ => UnknownSize
    }

    private def getVecArgs(cInfo: CircuitInfo, args: List[Tree]): (CSize, SignalType) = {
      if (args.length == 2) {
        val size      = KnownSize(STermLoader(cInfo, args.head).get._2.get)
        val cDataType = SignalTypeLoader(cInfo, args.tail.head).get
        (size, cDataType)
      } else {
        reporter.error(args.head.pos, "Unknow arg of Vec")
        (UnknownSize, SignalType.empty)
      }
    }

    def fromString(tpe: String): Option[SignalType] = {
      tpe match {
        case "chisel3.UInt" => Some(UInt.empty)
        case "chisel3.SInt" => Some(SInt.empty)
        case "chisel3.Bool" => Some(Bool.empty)
        case "chisel3.Data" => Some(UInt.empty)
        case _              => None
      }
    }

    def fromTpt(tree: Tree): Option[SignalType] = {
      val tpe            = autoTypeErasure(tree)
      val someSignalType = fromString(tpe.toString())
      if (someSignalType.isEmpty)
        reporter.error(tree.pos, s"unknow data type `${tpe}` in SignalTypeLoader.fromTpt")
      someSignalType
    }

    def apply(cInfo: CircuitInfo, tr: Tree): Option[SignalType] = {
      val tree = passThrough(tr)._1
      tree match {
        case Apply(fun, args) =>
          val someDirection = CDirectionLoader(fun)
          someDirection match {
            /* Apply(<Input(_)>, List(<UInt(width.W)>)) */
            case Some(direction) =>
              val tpe = SignalTypeLoader(cInfo, args.head)
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
                      val (size, sigType) = getVecArgs(cInfo, args)
                      Some(Vec(size, Node, sigType))
                    case _ =>
                      unprocessedTree(f, "SignalTypeLoader #1")
                      Some(SignalType.empty)
                  }
                case Select(New(tpt), termNames.CONSTRUCTOR) =>
                  val bundleFullName = tpt.tpe.toString()
                  val someBundleDef  = cInfo.readerInfo.bundleDefs.get(bundleFullName)
                  someBundleDef.map(_.bundle)
                case _ =>
                  unprocessedTree(f, "SignalTypeLoader #2")
                  Some(SignalType.empty)
              }
          }

        case Block(stats, _) =>
          Some(BundleDefLoader(cInfo, stats.head, "").get._2.get.bundle)
        case _ =>
          errorTree(tree, "SignalTypeLoader #3")
          Some(SignalType.empty)
      }
    }
  }

  object STypeLoader extends MTypeLoaderLib {

    private val wrappedTypes = List(
      "scala.collection.immutable.Range",
      "scala.collection.WithFilter[Any,[_]Any]",
      "Nothing"
    )
    private def isSeq(tpe: Type): Boolean = {
      val typeStr = tpe.toString()
      List(
        """IndexedSeq\[.*\]""",
        """Array\[.*\]""",
        """Seq\[.*\]"""
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
            assertWarning(
              wrappedTypes.contains(s),
              tr.pos,
              s"This type `${s}` need check"
            )
            Some(StWrapped(s))

        }
      }
    }
  }

  object MTypeLoader {
    def fromTpt(tr: Tree): Option[MType] = {
      if (isChiselSignalType(tr)) SignalTypeLoader.fromTpt(tr)
      else STypeLoader.fromTpt(tr)
    }
    def apply(cInfo: CircuitInfo, tr: Tree): Option[MType] = {
      if (isChiselSignalType(tr))
        SignalTypeLoader(cInfo, tr)
      else STypeLoader.fromTpt(tr)
    }
  }
}
