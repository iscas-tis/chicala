package chicala.sort

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.PluginComponent
import nsc.transform.TypingTransformers

import java.io._

import chicala.util.Format

object StatementSortComponent {
  val phaseName = "statementSort"
}

class StatementSortComponent(val global: Global) extends PluginComponent with TypingTransformers {
  import global._

  val runsAfter: List[String] = List("typer")
  // to keep recursive structure
  override val runsBefore: List[String] = List("tailcalls")

  val phaseName: String = StatementSortComponent.phaseName

  def newPhase(_prev: Phase)                = new StatementSortPhase(_prev)
  def newTransformer(unit: CompilationUnit) = new StatementSortTransformer(unit)

  class StatementSortPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      unit.body = newTransformer(unit).transform(unit.body)
    }
  }

  class StatementSortTransformer(unit: CompilationUnit)
      extends TypingTransformer(unit)
      with StatementProcess
      with Format {
    lazy val global: StatementSortComponent.this.global.type = StatementSortComponent.this.global

    val testRunDir = new File("test_run_dir/" + phaseName)
    testRunDir.mkdirs()

    val chicalaLog = new BufferedWriter(new PrintWriter(testRunDir.getPath() + "/chicala_log.txt"))
    global.computePhaseAssembly().foreach(s => chicalaLog.write(s.toString + "\n"))
    chicalaLog.close()

    val packageDef  = unit.body.asInstanceOf[PackageDef]
    val packageName = packageDef.pid.toString()

    override def transform(tree: Tree): Tree = tree match {
      case ClassDef(mods, name, tparams, Template(parents, self, body)) => {
        val classDefFile = new BufferedWriter(new PrintWriter(testRunDir.getPath() + s"/${packageName}.${name}.scala"))
        classDefFile.write(show(tree) + "\n")
        classDefFile.write("\n")
        classDefFile.write(showFormattedRaw(tree) + "\n")
        classDefFile.close()

        // Filter chisel statements
        // Analysis signal dependency for every statements
        val statements = Statements.fromTreeList(body)
        global.reporter.echo(
          s"flitered ${statements.body.length} statements form ${body.length} in ${packageName}.${name}"
        )
        // Mark invalid connect (couse by last connect semantics)
        val markedStatements = statements.markInvalidConnect()

        // Export dependency graph
        // Topological sort
        val dependencyGraph  = markedStatements.dependencyGraph
        val topologicalOrder = dependencyGraph.toplogicalSort(layer = true)

        // Reaplace AST
        val removeTrees = markedStatements.trees
        val insertTrees = markedStatements.generate(topologicalOrder.map(_.id))

        val newBody = body.filter(!removeTrees.contains(_)) ::: insertTrees
        val impl    = tree.asInstanceOf[ClassDef].impl

        val newClassDef = treeCopy.ClassDef(tree, mods, name, tparams, treeCopy.Template(impl, parents, self, newBody))

        val newClassDefFile = new BufferedWriter(
          new PrintWriter(testRunDir.getPath() + s"/${packageName}.${name}-sorted.scala")
        )
        newClassDefFile.write(show(newClassDef) + "\n")
        newClassDefFile.write("\n")
        newClassDefFile.write(showFormattedRaw(newClassDef) + "\n")
        newClassDefFile.close()

        newClassDef
      }
      case _ => super.transform(tree)
    }
  }
}
