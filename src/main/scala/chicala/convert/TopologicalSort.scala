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
        id: Id,
        statement: MStatement,
        last: Map[String, Set[Id]]
    ): Map[String, Set[Id]] = {

      def mergeTwoBranch(
          lastMapOne: Map[String, Set[Id]],
          lastMapTwo: Map[String, Set[Id]]
      ): Map[String, Set[Id]] = {
        lastMapTwo.foldLeft(lastMapOne) { case (m, (key, set)) =>
          if (m.contains(key)) m.updated(key, m(key) ++ set)
          else m.updated(key, set)
        }
      }

      statement match {
        case c: Connect =>
          vertexs += Vertex(id)
          val lefts = c.relatedSignals.fully
          lefts.map(last(_)).flatten.foreach(x => edges += DirectedEdge(Vertex(id), Vertex(x)))
          lefts.foldLeft(last)(_.updated(_, Set(id)))
        case md: MDef =>
          vertexs += Vertex(id)
          last ++ md.relatedSignals.fully
            .map(x => moduleDef.name.toString() + ".this." + x -> Set(id))
            .toMap
        case a: Assert =>
          vertexs += Vertex(id)
          last
        case w: When =>
          val whenMap  = getVertexAndLastConnectDependcyFromList(id :+ 1, w.whenBody, last)
          val otherMap = getVertexAndLastConnectDependcyFromList(id :+ 2, w.otherBody, last)
          otherMap.foldLeft(whenMap) { case (m, (key, set)) =>
            if (m.contains(key)) m.updated(key, m(key) ++ set)
            else m.updated(key, set)
          }
        case switch: Switch =>
          switch.branchs
            .map(_._2)
            .zip((1 to switch.branchs.size).map(id :+ _))
            .map({ case (body, subId) => getVertexAndLastConnectDependcyFromList(subId, body, last) })
            .foldLeft(last)(mergeTwoBranch(_, _))
        case sIf: SIf =>
          val thenLastMap = getVertexAndLastConnectDependcy(id :+ 1, sIf.thenp, last)
          val elseLastMap = getVertexAndLastConnectDependcy(id :+ 2, sIf.elsep, last)
          mergeTwoBranch(thenLastMap, elseLastMap)
        case sBlock: SBlock =>
          getVertexAndLastConnectDependcyFromList(id, sBlock.body, last)
        case EmptyMTerm =>
          last
        case _ =>
          println(
            s"(-_-) not processed ${moduleDef.name} statement in "
              + "ToplogicalSort.getDependencyGraph.getVertexAndLastConnectDependcy: " +
              s"${statement.toString()}"
          )
          last
      }
    }

    def getVertexAndLastConnectDependcyFromList(
        idPrefix: Id,
        statements: List[MStatement],
        lastConnect: Map[String, Set[Id]]
    ): Map[String, Set[Id]] = {
      statements
        .zip((1 to statements.length).map(idPrefix :+ _))
        .foldLeft(lastConnect)({ case (last, (statement, id)) =>
          getVertexAndLastConnectDependcy(id, statement, last)
        })
    }

    def getConnectDependcy(
        id: Id,
        statement: MStatement,
        lastConnect: Map[String, Set[Id]],
        dependency: Set[String]
    ): Unit = {
      statement match {
        case c: Connect =>
          val left = c.relatedSignals.fully.head
          if (lastConnect(left).contains(id)) { // only valid connection
            edges ++= (dependency ++ c.relatedSignals.dependency)
              .map(lastConnect(_))
              .flatten
              .map(x => DirectedEdge(Vertex(id), Vertex(x)))
          }
        case w: When =>
          val newDependency = dependency ++ w.cond.relatedSignals.dependency
          getConnectDependcyFromList(id :+ 1, w.whenBody, lastConnect, newDependency)
          getConnectDependcyFromList(id :+ 2, w.otherBody, lastConnect, newDependency)
        case sIf: SIf =>
          getConnectDependcy(id :+ 1, sIf.thenp, lastConnect, dependency)
          getConnectDependcy(id :+ 2, sIf.elsep, lastConnect, dependency)
        case switch: Switch =>
          /** dependency with `switch.cond`. `v` for each `branch` should be
            * `Lit` that has no dependency
            */
          val newDependency = dependency ++ switch.cond.relatedSignals.dependency
          switch.branchs
            .zip((1 to switch.branchs.size).map(id :+ _))
            .foreach { case ((v, body), idPrefix) =>
              getConnectDependcyFromList(idPrefix, body, lastConnect, newDependency)
            }
        case s =>
          edges ++= (dependency ++ s.relatedSignals.dependency)
            .map(lastConnect(_))
            .flatten
            .map(x => DirectedEdge(Vertex(id), Vertex(x)))
      }
    }

    def getConnectDependcyFromList(
        idPrefix: Id,
        statements: List[MStatement],
        lastConnect: Map[String, Set[Id]],
        dependency: Set[String]
    ): Unit = {
      statements
        .zip((1 to statements.length).map(idPrefix :+ _))
        .foreach { case (statement, id) =>
          getConnectDependcy(id, statement, lastConnect, dependency)
        }
    }

    val lastConnect = getVertexAndLastConnectDependcyFromList(Id.empty, moduleDef.body, Map.empty)
    getConnectDependcyFromList(Id.empty, moduleDef.body, lastConnect, Set.empty)

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

    def doReorder(bodyList: List[MStatement], idList: List[Id]): List[MStatement] = {
      val body = bodyList.toArray

      mergeId(idList).map { case (index, restList) =>
        body(index - 1) match {
          case _: Connect | _: Assert | _: CValDef => body(index - 1)
          case w: When =>
            val merged = mergeId(restList).toMap
            val whenBody =
              if (merged.contains(1)) doReorder(w.whenBody, merged(1))
              else List.empty
            val otherBody =
              if (merged.contains(2)) doReorder(w.otherBody, merged(2))
              else List.empty
            val hasElseWhen =
              if (w.hasElseWhen && otherBody.nonEmpty) true
              else false
            When(w.cond, whenBody, otherBody, hasElseWhen)
          case b: BulkConnect =>
            reporter.echo(s"(-_-) not processed in ToplogicalSort.doReorder: ${b}")
            b
          case s => s
        }
      }
    }

    ModuleDef(moduleDef.name, moduleDef.vparams, doReorder(moduleDef.body, topologicalOrder))
  }

  def dependencySort(moduleDef: ModuleDef): ModuleDef = {
    val dependencyGraph  = getDependencyGraph(moduleDef)
    val topologicalOrder = dependencyGraph.toplogicalSort(layer = true)
    Format.saveToFile(
      s"./test_run_dir/chiselToScala/${moduleDef.name}.dot",
      dependencyGraph.toDot
    )
    Format.saveToFile(
      s"./test_run_dir/chiselToScala/${moduleDef.name}.order.scala",
      topologicalOrder.toString()
    )
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

  def toPointString: String = {
    if (seq.isEmpty) ""
    else seq.map(_.toString()).reduce(_ + "." + _)
  }
  def toNameString: String = {
    s"p${toPointString.replace(".", "x")}"
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

  def toDot: String = {
    val nodes = vertexs.map(_.id).map(x => s"${x.toNameString} [label=\"${x.toPointString}\"]")
    val diedges = edges.map { case DirectedEdge(from, to) =>
      val fromNode = from.id.toNameString
      val toNode   = to.id.toNameString
      s"${fromNode}->${toNode}"
    }

    s"""digraph g{
      |${nodes.map(x => "  " + x + "\n").reduce(_ + _)}
      |${diedges.map(x => "  " + x + "\n").reduce(_ + _)}
      |}""".stripMargin
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
