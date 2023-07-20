package chicala.convert

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst

trait ToplogicalSort { self: ChicalaAst =>
  val global: Global
  import global._

  def getDependencyGraph(moduleDef: ModuleDef): DirectedGraph = {
    import scala.collection.mutable

    val vertexs = mutable.Set.empty[Vertex]
    val edges   = mutable.Set.empty[DirectedEdge]

    def getVertexAndLastConnectDependcy(
        idPrefix: Id,
        statements: List[CStatement],
        lastConnect: Map[String, Set[Id]]
    ): Map[String, Set[Id]] = {
      var last = lastConnect
      (1 to statements.length).map(idPrefix :+ _).zip(statements).foreach { case (id, statement) =>
        statement match {
          case c: Connect =>
            vertexs += Vertex(id)
            val left = c.relatedSignals.fully.head
            last(left).foreach(x => edges += DirectedEdge(Vertex(id), Vertex(x)))
            last = last.updated(left, Set(id))
          case w: When =>
            val whenMap  = getVertexAndLastConnectDependcy(id :+ 1, w.whenBody, last)
            val otherMap = getVertexAndLastConnectDependcy(id :+ 2, w.otherBody, last)
            last = otherMap.foldLeft(whenMap) { case (m, (key, set)) =>
              if (m.contains(key)) m.updated(key, m(key) ++ set)
              else m.updated(key, set)
            }
          case i: IoDef => // TODO: use SignalDef
            vertexs += Vertex(id)
            last = last ++ i.relatedSignals.fully.map(x => i.circuitName.toString() + ".this." + x -> Set(id)).toMap
          case w: WireDef =>
            vertexs += Vertex(id)
            last = last ++ w.relatedSignals.fully.map(x => w.circuitName.toString() + ".this." + x -> Set(id)).toMap
          case a: Assert =>
            vertexs += Vertex(id)
          case _ => // TODO: remove _
            println(
              "(-_-) not processed in "
                + "ToplogicalSort.getDependencyGraph.getVertexAndLastConnectDependcy: " +
                s"${statement.toString()}"
            )
        }
      }
      last
    }

    def getConnectDependcy(
        idPrefix: Id,
        statements: List[CStatement],
        lastConnect: Map[String, Set[Id]],
        dependency: Set[String]
    ): Unit = {
      (1 to statements.length).map(idPrefix :+ _).zip(statements).foreach { case (id, statement) =>
        statement match {
          case c: Connect =>
            val left = c.relatedSignals.fully.head
            if (lastConnect.contains(left)) { // valid connection
              edges ++= (dependency ++ c.relatedSignals.dependency)
                .map(lastConnect(_))
                .flatten
                .map(x => DirectedEdge(Vertex(id), Vertex(x)))
            }
          case w: When =>
            getConnectDependcy(id :+ 1, w.whenBody, lastConnect, dependency ++ w.cond.signals)
            getConnectDependcy(id :+ 2, w.otherBody, lastConnect, dependency ++ w.cond.signals)
          case a: Assert =>
            edges ++= (dependency ++ a.relatedSignals.dependency)
              .map(lastConnect(_))
              .flatten
              .map(x => DirectedEdge(Vertex(id), Vertex(x)))
          case _: IoDef | _: WireDef =>
          case _ =>
            reporter.echo(
              "(-_-) not processed in "
                + "ToplogicalSort.getDependencyGraph.getConnectDependcy: " +
                s"${statement.toString()}"
            )
        }
      }
    }

    val lastConnect = getVertexAndLastConnectDependcy(Id.empty, moduleDef.body, Map.empty)
    getConnectDependcy(Id.empty, moduleDef.body, lastConnect, Set.empty)

    DirectedGraph(vertexs.toSet, edges.toSet)
  }

  def reorder(moduleDef: ModuleDef, topologicalOrder: List[Id]) = {

    def mergeId(idList: List[Id]): List[(Int, List[Id])] = {
      // merge adjacent IDs have same first-level index
      // return list of merged IDs, List((top, List(rest, ...)), ...)
      idList
        .foldLeft(List.empty[(Int, List[Id])]) { case (ls, id) =>
          ls match {
            case Nil => List((id.top, List(id.rest)))
            case (index, restList) :: next =>
              if (index == id.top)
                (index, id.rest :: restList) :: next // reversed append 1
              else
                (id.top, List(id.rest)) :: ls // reversed append 2
          }
        }
        .map(x => (x._1, x._2.reverse)) // reverse 1
        .reverse                        // reverse 2
    }

    def doReorder(bodyList: List[CStatement], idList: List[Id]): List[CStatement] = {
      val body = bodyList.toArray

      mergeId(idList).map { case (index, restList) =>
        body(index - 1) match {
          case _: Connect | _: Assert | _: SignalDef => body(index - 1)
          case w: When =>
            val merged = mergeId(restList).toMap
            val whenBody =
              if (merged.contains(1)) doReorder(w.whenBody, merged(1))
              else List.empty
            val otherBody =
              if (merged.contains(2)) doReorder(w.otherBody, merged(2))
              else List.empty
            When(w.cond, whenBody, otherBody)
          case b: BulkConnect =>
            reporter.echo(s"(-_-) not processed in ToplogicalSort.doReorder: ${b}")
            b
        }
      }
    }

    ModuleDef(moduleDef.name, moduleDef.info, doReorder(moduleDef.body, topologicalOrder))
  }

  def dependencySort(moduleDef: ModuleDef): ModuleDef = {
    val dependencyGraph  = getDependencyGraph(moduleDef)
    val topologicalOrder = dependencyGraph.toplogicalSort(layer = true)
    reorder(moduleDef, topologicalOrder)
  }
}

case class Id(val seq: List[Int]) extends Ordered[Id] {
  def compare(that: Id): Int = compareSeq(this.seq, that.seq)

  private def compareSeq(seqA: List[Int], seqB: List[Int]): Int = {
    (seqA, seqB) match {
      case (Nil, Nil) => 0
      case (_, Nil)   => 1
      case (Nil, _)   => -1
      case (_, _) =>
        if (seqA.head == seqB.head) compareSeq(seqA.tail, seqB.tail)
        else seqA.head compare seqB.head
    }
  }

  override def toString(): String = {
    if (seq.isEmpty) "Id()"
    else s"Id(${seq.map(_.toString()).reduce(_ + ", " + _)})"
  }

  def :+(number: Int): Id = Id(seq :+ number)

  def top: Int = seq.head
  def rest: Id = Id(seq.tail)
}
object Id {
  def empty = Id(List.empty)
}
case class Vertex(val id: Id) extends Ordered[Vertex] {
  def compare(that: Vertex): Int = id.compare(that.id)
}
case class DirectedEdge(val from: Vertex, val to: Vertex) extends Ordered[DirectedEdge] {
  def compare(that: DirectedEdge): Int = {
    val fromCompare = from.compare(that.from)
    if (fromCompare == 0) to.compare(that.to)
    else fromCompare
  }
}
case class DirectedGraph(val vertexs: Set[Vertex], edges: Set[DirectedEdge]) {

  override def toString(): String = {
    val vertexsSetName = if (vertexs.size <= 4) "Set" else "HashSet"
    val vertexsList    = vertexs.toList.sorted.map(_.toString()).reduce(_ + ", " + _)
    val edgesSetName   = if (edges.size <= 4) "Set" else "HashSet"
    val edgesList      = edges.toList.sorted.map(_.toString()).reduce(_ + ", " + _)
    s"DirectedGraph($vertexsSetName($vertexsList),$edgesSetName($edgesList))"
  }

  def toplogicalSort(layer: Boolean = false): List[Id] = {
    import scala.collection.mutable

    val incoming        = mutable.Map.from(vertexs.map(_ -> mutable.Set.empty[Vertex]))
    val dependencyCount = mutable.Map.from(vertexs.map(_ -> 0))
    edges.foreach { case DirectedEdge(from, to) =>
      incoming(to) += from
      dependencyCount(from) += 1
    }

    val queue   = mutable.PriorityQueue.from(vertexs.filter(dependencyCount(_) == 0)).reverse
    var revList = List.empty[Id] // store reversed toplogical order
    while (queue.nonEmpty) {
      val thisLayer = if (layer) queue.dequeueAll else List(queue.dequeue())
      thisLayer.foreach { v =>
        revList = v.id :: revList
        incoming(v).foreach { u =>
          dependencyCount(u) -= 1
          if (dependencyCount(u) == 0)
            queue += u
        }
      }
    }

    revList.reverse
  }
}
object DirectedGraph {
  def empty = DirectedGraph(Set.empty, Set.empty)
}
