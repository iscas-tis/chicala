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

      val fw = new BufferedWriter(new PrintWriter(testRunDir.getPath() + "/test.scala"))

      val packageDef  = unit.body.asInstanceOf[PackageDef]
      val packageName = packageDef.pid.toString()

      for (tree @ ClassDef(mods, name, tparams, Template(parents, self, body)) <- packageDef.stats) {
        val sorter = new TopologicalSort[global.type]
        import sorter.{global => _, _}

        // Filter chisel statements
        // TODO: Analysis signal dependency
        val statements = Statements.fromTreeList(body)
        global.reporter.echo(
          s"flitered ${statements.body.length} statements form ${body.length} in ${packageName}.${name}"
        )
        statements.body.foreach(x => {
          fw.write(show(x.tree) + "\n")
          fw.write(showFormattedRaw(x.tree) + "\n")
          fw.write("\n")
        })

        // TODO: Mark invalid connetion (couse by last connect semantics)
        // TODO: Expand blocks
        // TODO: Export dependency graph
        // TODO: Topological sort
        // TODO: Merge
        // TODO: Return new AST
      }

      fw.close()

      chicalaLog.close()
    }
  }
}
