package chicala.ast

import scala.tools.nsc.Global

trait CircuitInfos { self: ChicalaAst =>
  val global: Global
  import global._

  case class CircuitInfo(
      val name: TypeName,
      val signal: Map[TermName, SignalInfo],
      val param: Map[TermName, TypeTree],
      val function: Map[TermName, TypeTree]
  ) {
    def updatedSignal(termName: TermName, signalInfo: SignalInfo): CircuitInfo =
      new CircuitInfo(name, signal + (termName -> signalInfo), param, function)
    def updatedSignals(signals: List[(TermName, SignalInfo)]): CircuitInfo =
      new CircuitInfo(name, signal ++ signals, param, function)

    def updatedParam(termName: TermName, typeTree: TypeTree): CircuitInfo =
      new CircuitInfo(name, signal, param + (termName -> typeTree), function)
    def updatedParams(params: List[(TermName, TypeTree)]): CircuitInfo =
      new CircuitInfo(name, signal, param ++ params, function)

    def updatedFuncion(termName: TermName, typeTree: TypeTree): CircuitInfo =
      new CircuitInfo(name, signal, param, function + (termName -> typeTree))

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
    def apply(name: TypeName) = new CircuitInfo(name, Map.empty, Map.empty, Map.empty)

    def empty = new CircuitInfo(TypeName(""), Map.empty, Map.empty, Map.empty)
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
