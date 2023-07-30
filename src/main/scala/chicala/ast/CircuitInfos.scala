package chicala.ast

import scala.tools.nsc.Global

trait CircuitInfos { self: ChicalaAst =>
  val global: Global
  import global._

  case class CircuitInfo(
      val name: TypeName,
      val signal: Map[TermName, SignalInfo],
      val param: Map[TermName, TypeTree],
      val function: Map[TermName, TypeTree],
      /* use to record EnumDef  */
      val numTmp: Int,
      val enumTmp: Option[(TermName, EnumDef)],
      val tupleTmp: Option[(TermName, STupleUnapplyDef)]
  ) {
    def updatedSignal(termName: TermName, signalInfo: SignalInfo): CircuitInfo =
      this.copy(signal = signal + (termName -> signalInfo))
    def updatedSignals(signals: List[(TermName, SignalInfo)]): CircuitInfo =
      this.copy(signal = signal ++ signals)

    def updatedParam(termName: TermName, typeTree: TypeTree): CircuitInfo =
      this.copy(param = param + (termName -> typeTree))
    def updatedParams(params: List[(TermName, TypeTree)]): CircuitInfo =
      this.copy(param = param ++ params)

    def updatedFuncion(termName: TermName, typeTree: TypeTree): CircuitInfo =
      this.copy(function = function + (termName -> typeTree))

    def updatedEnumTmp(num: Int, et: Option[(TermName, EnumDef)]): CircuitInfo =
      this.copy(numTmp = num, enumTmp = et)
    def updatedTupleTmp(num: Int, tt: Option[(TermName, STupleUnapplyDef)]): CircuitInfo =
      this.copy(numTmp = num, tupleTmp = tt)

    def contains(termName: TermName): Boolean =
      signal.contains(termName) || param.contains(termName) || function.contains(termName)
    def contains(tree: Tree): Boolean = {
      tree match {
        case Select(This(this.name), termName: TermName) => contains(termName)
        case _ =>
          unprocessedTree(tree, "CircuitInfo.contains")
          false
      }
    }

    def getSignalInfo(tree: Tree): SignalInfo = {
      def select(signalInfo: SignalInfo, termName: TermName): SignalInfo = signalInfo match {
        case SignalInfo(physicalType, dataType) =>
          dataType match {
            case Bundle(signals) if signals.contains(termName) =>
              SignalInfo(physicalType, signals(termName))
            case _ => {
              reporter.error(tree.pos, s"TermName ${termName} not found in ${dataType}")
              SignalInfo.empty
            }
          }
      }
      tree match {
        case Select(This(this.name), termName: TermName) => signal(termName)
        case Select(qualifier, termName: TermName)       => select(getSignalInfo(qualifier), termName)
        case Ident(termName: TermName)                   => signal(termName)
        case _ => {
          unprocessedTree(tree, "CircuitInfo.getSignalInfo")
          reporter.error(tree.pos, s"CircuitInfo.getSignalInfo not process")
          SignalInfo.empty
        }
      }
    }

    def getFunctionInfo(tree: Tree): TypeTree = {
      tree match {
        case Select(This(this.name), termName: TermName) => function(termName)
      }
    }
  }

  object CircuitInfo {
    def apply(name: TypeName) = new CircuitInfo(name, Map.empty, Map.empty, Map.empty, 0, None, None)

    def empty = new CircuitInfo(TypeName(""), Map.empty, Map.empty, Map.empty, 0, None, None)
  }

  case class RelatedSignals(val fully: Set[String], val partially: Set[String], val dependency: Set[String]) {
    def ++(that: RelatedSignals): RelatedSignals = {
      RelatedSignals(
        this.fully ++ that.fully,
        this.partially ++ that.partially,
        this.dependency ++ that.dependency
      )
    }
  }
  object RelatedSignals {
    def empty = RelatedSignals(Set.empty, Set.empty, Set.empty)
  }
}
