package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst
import chicala.ast.util.Transformers

trait DependencySorts extends ChicalaPasss with Transformers { self: ChicalaAst =>
  val global: Global
  import global._

  object DependencySort extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef => dependencySort(m)
        case x            => x
      }
    }

    def getDependencyGraph(body: List[MStatement], isModuleTop: Boolean = false)(implicit
        moduleName: String
    ): DirectedGraph = {
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
          case _: Assert | _: CApply =>
            vertexs += Vertex(id)
            last
          case c: Connect =>
            vertexs += Vertex(id)
            updatedLast(last, id, c.relatedIdents.fully)
          case _: MDef | _: SubModuleRun =>
            vertexs += Vertex(id)
            updatedLast(
              last,
              id,
              if (isModuleTop)
                statement.relatedIdents.fully
                  .map(moduleName + ".this." + _)
              else
                statement.relatedIdents.fully
            )
          case w: When =>
            val whenLast  = getVertexAndLastConnectDependcy(id :+ 1, w.whenp, last)
            val otherLast = getVertexAndLastConnectDependcy(id :+ 2, w.otherp, last)
            mergedTwoBranchLast(whenLast, otherLast)
          case switch: Switch =>
            id.asPrefixZipWith(
              switch.branchs
                .map(_._2)
            ).map({ case (subId, branchp) => getVertexAndLastConnectDependcy(subId, branchp, last) })
              .foldLeft(last)(mergedTwoBranchLast(_, _))

          case _: STuple =>
            vertexs += Vertex(id)
            last
          case sApply: SApply =>
            vertexs += Vertex(id)
            updatedLast(last, id, sApply.relatedIdents.fully)
          case sIf: SIf =>
            val thenLast = getVertexAndLastConnectDependcy(id :+ 1, sIf.thenp, last)
            val elseLast = getVertexAndLastConnectDependcy(id :+ 2, sIf.elsep, last)
            mergedTwoBranchLast(thenLast, elseLast)
          case sBlock: SBlock =>
            getVertexAndLastConnectDependcyFromList(id, sBlock.body, last)
          case sAssign: SAssign =>
            vertexs += Vertex(id)
            last

          case EmptyMTerm =>
            last
          case _ =>
            reporter.error(
              NoPosition,
              s"Not processed ${moduleName} statement in " +
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
        idPrefix
          .asPrefixZipWith(statements)
          .foldLeft(lastConnect)({ case (last, (id, statement)) =>
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
            getConnectDependcy(id :+ 1, w.whenp, lastConnect, newDependency)
            getConnectDependcy(id :+ 2, w.otherp, lastConnect, newDependency)
          case sIf: SIf =>
            getConnectDependcy(id :+ 1, sIf.thenp, lastConnect, dependency)
            getConnectDependcy(id :+ 2, sIf.elsep, lastConnect, dependency)
          case switch: Switch =>
            /** dependency with `switch.cond`. `v` for each `branch` should be
              * `Lit` that has no dependency
              */
            val newDependency = dependency ++ switch.cond.relatedIdents.dependency
            id.asPrefixZipWith(
              switch.branchs
            ).foreach { case (subId, (v, branchp)) =>
              getConnectDependcy(subId, branchp, lastConnect, newDependency)
            }
          case sBlock: SBlock =>
            getConnectDependcyFromList(id, sBlock.body, lastConnect, dependency)
          case EmptyMTerm =>
          case s =>
            edges ++= (dependency ++ s.relatedIdents.dependency)
              .map(lastConnect.getOrElse(_, Set.empty))
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
        idPrefix
          .asPrefixZipWith(statements)
          .foreach { case (id, statement) =>
            getConnectDependcy(id, statement, lastConnect, dependency)
          }
      }

      val lastConnect = getVertexAndLastConnectDependcyFromList(Id.empty, body, Map.empty)
      getConnectDependcyFromList(Id.empty, body, lastConnect, Set.empty)
      // getValVarDependcy

      DirectedGraph(vertexs.toSet, edges.toSet)
    }

    def reorder(body: List[MStatement], topologicalOrder: List[Id]) = {

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
                  val whenp =
                    if (mergedOne.contains(1)) doReorder(w.whenp, mergedOne(1)).head
                    else EmptyMTerm
                  val otherp =
                    if (mergedOne.contains(2)) doReorder(w.otherp, mergedOne(2)).head
                    else EmptyMTerm
                  val hasElseWhen =
                    if (w.hasElseWhen && otherp.nonEmpty) true
                    else false
                  When(w.cond, whenp, otherp, hasElseWhen)
                }
              case sIf: SIf =>
                val merged = mergeId(restList)
                val parts  = splitParts(merged, 2)

                parts.map(_.toMap).map { mergedOne =>
                  // FIXME: use MTerm, use SBlock wrap SDef
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
                    val body = doReorder(branchsArr(index - 1)._2, rest)
                    assert(body.size == 1, "should have only one statement") // FIXME
                    (branchsArr(index - 1)._1, body.head)
                  }
                  Switch(switch.cond, branchs)
                }

              case sBlock: SBlock =>
                doReorderList(sBlock.body, restList) match {
                  case Nil         => List(EmptyMTerm)
                  case head :: Nil => List(head)
                  case ls          => List(SBlock(ls, sBlock.tpe))
                }
              case s =>
                reporter.error(
                  NoPosition,
                  s"Not processed in ToplogicalSort.doReorder: ${s}\n${restList}"
                )
                List()
            }
        }
      }

      doReorderList(body, topologicalOrder)
    }

    class ReorderSubFieldTransFormer(implicit moduleName: String) extends Transformer {
      def reorderTopSBlockOrOther(bodyp: MStatement): MStatement = {
        bodyp match {
          case SBlock(body, tpe) => SBlock(reorderTopList(body), tpe)
          case x                 => transform(x)
        }
      }
      override def transform(mStatement: MStatement): MStatement = mStatement match {
        case s @ SDefDef(_, _, _, defp) =>
          s.copy(defp = reorderTopSBlockOrOther(defp))
        case f @ SFunction(_, funcp) =>
          f.copy(funcp = reorderTopSBlockOrOther(funcp).asInstanceOf[MTerm])

        case x => super.transform(x)
      }
    }

    def reorderTopList(body: List[MStatement], isModuleTop: Boolean = false)(implicit
        moduleName: String
    ): List[MStatement] = {
      val reorderSubField  = new ReorderSubFieldTransFormer
      val newBody          = body.map(reorderSubField(_))
      val dependencyGraph  = getDependencyGraph(newBody, isModuleTop)
      val topologicalOrder = dependencyGraph.toplogicalSort(layer = false)
      reorder(newBody, topologicalOrder)
    }

    def dependencySort(moduleDef: ModuleDef): ModuleDef = {
      moduleDef.copy(body = reorderTopList(moduleDef.body, true)(moduleDef.name.toString()))
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

  def asPrefixZipWith[T](seq: Seq[T]): Seq[(Id, T)] = {
    seq.zipWithIndex.map({ case (t, i) => (this :+ (i + 1), t) })
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
