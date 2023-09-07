package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst

trait DependencySorts extends ChicalaPasss { self: ChicalaAst =>
  val global: Global
  import global._

  object DependencySort extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef => dependencySort(m)
        case x            => x
      }
    }

    def getDependencyGraph(moduleDef: ModuleDef): DirectedGraph = {
      import scala.collection.mutable

      val vertexs = mutable.Set.empty[Vertex]
      val edges   = mutable.Set.empty[DirectedEdge]

      def getVertexAndLastConnectDependcy(
          id: Id,
          statement: MStatement,
          last: Map[String, Set[Id]]
      ): Map[String, Set[Id]] = {

        def mergedTwoBranchLast(
            lastMapOne: Map[String, Set[Id]],
            lastMapTwo: Map[String, Set[Id]]
        ): Map[String, Set[Id]] = {
          lastMapTwo.foldLeft(lastMapOne) { case (m, (key, set)) =>
            if (m.contains(key)) m.updated(key, m(key) ++ set)
            else m.updated(key, set)
          }
        }
        def updatedLast(
            last: Map[String, Set[Id]],
            id: Id,
            signals: Set[String]
        ): Map[String, Set[Id]] = {
          val lastIds = signals.map(last.getOrElse(_, List())).flatten
          edges ++= lastIds.map(x => DirectedEdge(Vertex(id), Vertex(x)))

          signals.foldLeft(last)(_.updated(_, Set(id))) // overwrite `signals` last connection id
        }

        statement match {
          case c: Connect =>
            vertexs += Vertex(id)
            updatedLast(last, id, c.relatedIdents.fully)
          case _: MDef | _: SubModuleRun =>
            vertexs += Vertex(id)
            updatedLast(
              last,
              id,
              statement.relatedIdents.fully
                .map(moduleDef.name.toString() + ".this." + _)
            )
          case a: Assert =>
            vertexs += Vertex(id)
            last
          case w: When =>
            val whenLast  = getVertexAndLastConnectDependcyFromList(id :+ 1, w.whenBody, last)
            val otherLast = getVertexAndLastConnectDependcyFromList(id :+ 2, w.otherBody, last)
            mergedTwoBranchLast(whenLast, otherLast)
          case switch: Switch =>
            switch.branchs
              .map(_._2)
              .zip((1 to switch.branchs.size).map(id :+ _))
              .map({ case (body, subId) => getVertexAndLastConnectDependcyFromList(subId, body, last) })
              .foldLeft(last)(mergedTwoBranchLast(_, _))

          case sApply: SApply =>
            vertexs += Vertex(id)
            updatedLast(last, id, sApply.relatedIdents.fully)
          case sIf: SIf =>
            val thenLast = getVertexAndLastConnectDependcy(id :+ 1, sIf.thenp, last)
            val elseLast = getVertexAndLastConnectDependcy(id :+ 2, sIf.elsep, last)
            mergedTwoBranchLast(thenLast, elseLast)
          case sBlock: SBlock =>
            getVertexAndLastConnectDependcyFromList(id, sBlock.body, last)
          case EmptyMTerm =>
            last
          case _ =>
            reporter.error(
              NoPosition,
              s"Not processed ${moduleDef.name} statement in " +
                "ToplogicalSort.getDependencyGraph.getVertexAndLastConnectDependcy:\n" +
                s"  ${statement.toString()}"
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
            val left = c.relatedIdents.fully.head
            if (lastConnect(left).contains(id)) { // only valid connection
              edges ++= (dependency ++ c.relatedIdents.dependency)
                .map(lastConnect(_))
                .flatten
                .map(x => DirectedEdge(Vertex(id), Vertex(x)))
            }
          case w: When =>
            val newDependency = dependency ++ w.cond.relatedIdents.dependency
            getConnectDependcyFromList(id :+ 1, w.whenBody, lastConnect, newDependency)
            getConnectDependcyFromList(id :+ 2, w.otherBody, lastConnect, newDependency)
          case sIf: SIf =>
            getConnectDependcy(id :+ 1, sIf.thenp, lastConnect, dependency)
            getConnectDependcy(id :+ 2, sIf.elsep, lastConnect, dependency)
          case switch: Switch =>
            /** dependency with `switch.cond`. `v` for each `branch` should be
              * `Lit` that has no dependency
              */
            val newDependency = dependency ++ switch.cond.relatedIdents.dependency
            switch.branchs
              .zip((1 to switch.branchs.size).map(id :+ _))
              .foreach { case ((v, body), idPrefix) =>
                getConnectDependcyFromList(idPrefix, body, lastConnect, newDependency)
              }
          case EmptyMTerm =>
          case s =>
            edges ++= (dependency ++ s.relatedIdents.dependency)
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
      def splitParts(
          mergeIds: List[(Int, List[Id])],
          maxPartIndex: Int
      ): List[List[(Int, List[Id])]] = {
        var parts     = List.empty[List[(Int, List[Id])]]
        var lastIndex = maxPartIndex
        mergeIds.foreach { x =>
          if (lastIndex >= x._1) parts = List(x) :: parts // reversed append #1
          else parts = (x :: parts.head) :: parts.tail    // reversed append #2
          lastIndex = x._1
        }
        parts.map(_.reverse).reverse // reverse #1 #2
      }

      def doReorderList(bodyList: List[MStatement], idList: List[Id]): List[MStatement] = {
        val body = bodyList.toArray
        mergeId(idList).map { case (index, restList) =>
          doReorder(body(index - 1), restList)
        }.flatten
      }
      def doReorder(mStatement: MStatement, restList: List[Id]): List[MStatement] = {
        restList match {
          case Nil | List(Id(Nil)) => List(mStatement)
          case _ =>
            mStatement match {
              case w: When =>
                val merged = mergeId(restList)
                val parts  = splitParts(merged, 2)
                parts.map(_.toMap).map { mergedOne =>
                  val whenBody =
                    if (mergedOne.contains(1)) doReorderList(w.whenBody, mergedOne(1))
                    else List.empty
                  val otherBody =
                    if (mergedOne.contains(2)) doReorderList(w.otherBody, mergedOne(2))
                    else List.empty
                  val hasElseWhen =
                    if (w.hasElseWhen && otherBody.nonEmpty) true
                    else false
                  When(w.cond, whenBody, otherBody, hasElseWhen)
                }
              case sIf: SIf =>
                val merged = mergeId(restList)
                val parts  = splitParts(merged, 2)

                parts.map(_.toMap).map { mergedOne =>
                  val thenp =
                    if (mergedOne.contains(1)) doReorder(sIf.thenp, mergedOne(1)).head.asInstanceOf[MTerm]
                    else EmptyMTerm
                  val elsep =
                    if (mergedOne.contains(2)) doReorder(sIf.elsep, mergedOne(2)).head.asInstanceOf[MTerm]
                    else EmptyMTerm
                  SIf(sIf.cond, thenp, elsep, sIf.tpe)
                }
              case switch: Switch =>
                val merged     = mergeId(restList)
                val parts      = splitParts(merged, switch.branchs.size)
                val branchsArr = switch.branchs.toIndexedSeq
                parts.map { ls =>
                  val branchs = ls.map { case (index, rest) =>
                    val body = doReorderList(branchsArr(index - 1)._2, rest)
                    (branchsArr(index - 1)._1, body)
                  }
                  Switch(switch.cond, branchs)
                }

              case sBlock: SBlock =>
                doReorderList(sBlock.body, restList)
              case s =>
                reporter.error(
                  NoPosition,
                  s"Not processed in ToplogicalSort.doReorder: ${s}\n${restList}"
                )
                List()
            }
        }
      }

      moduleDef.copy(body = doReorderList(moduleDef.body, topologicalOrder))
    }

    def dependencySort(moduleDef: ModuleDef): ModuleDef = {
      val pathPrefix = s"./test_run_dir/chiselToScala/test/${moduleDef.fullName.replace('.', '/')}"

      Format.saveToFile(
        s"${pathPrefix}.related.scala",
        moduleDef.body
          .map(s => s.toString() + "\n" + s.relatedIdents + "\n\n")
          .fold("")(_ + _)
      )

      val dependencyGraph  = getDependencyGraph(moduleDef)
      val topologicalOrder = dependencyGraph.toplogicalSort(layer = false)

      Format.saveToFile(s"${pathPrefix}.dot", dependencyGraph.toDot)
      Format.saveToFile(s"${pathPrefix}.order.scala", topologicalOrder.toString())

      reorder(moduleDef, topologicalOrder)
    }
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
      |${nodes.map(x => "  " + x + "\n").foldLeft("")(_ + _)}
      |${diedges.map(x => "  " + x + "\n").foldLeft("")(_ + _)}
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