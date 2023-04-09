package chicala.sort

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.PluginComponent

import java.io._

import chicala.util.Format

object StatementSortComponent {
  val phaseName = "statementSort"
}

class StatementSortComponent(val global: Global) extends PluginComponent {
  implicit private val implicitGlobal: global.type = global
  import global._

  private val fmt = new Format
  import fmt._

  val runsAfter: List[String] = List("typer")
  // to keep recursive structure
  override val runsBefore: List[String] = List("tailcalls")

  val phaseName: String      = StatementSortComponent.phaseName
  def newPhase(_prev: Phase) = new StatementSortPhase(_prev)

  class StatementSortPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      val testRunDir = new File("test_run_dir/" + phaseName)
      testRunDir.mkdirs()

      val chicalaLog = new BufferedWriter(new PrintWriter(testRunDir.getPath() + "/chicala_log.txt"))
      global.computePhaseAssembly().foreach(s => chicalaLog.write(s.toString + "\n"))

      val packageDef  = unit.body.asInstanceOf[PackageDef]
      val packageName = packageDef.pid.toString()

      for (tree @ ClassDef(mods, name, tparams, Template(parents, self, body)) <- packageDef.stats) {
        val fw = new BufferedWriter(new PrintWriter(testRunDir.getPath() + s"/${packageName}.${name}.scala"))
        fw.write(show(tree) + "\n")
        fw.write("\n")
        for (bodytree <- body) {
          fw.write("bodytree:\n")
          fw.write(show(bodytree) + "\n")
          fw.write(showFormattedRaw(bodytree) + "\n")
          fw.write("\n")
        }
        fw.close()

        val processor = new StatementProcess[global.type]
        import processor.{global => _, _}

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

        // TODO: Merge
        // TODO: Return new AST
      }

      chicalaLog.close()
    }
  }
}
