package chicala.sort

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.PluginComponent

import java.io._

import chicala.util.Format._

object StatementSortComponent {
  val phaseName = "statementSort"
}

class StatementSortComponent(val global: Global) extends PluginComponent {
  import global._

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
      for (tree @ ClassDef(mods, name, tparams, Template(parents, self, body)) <- unit.body) {
        fw.write(show(tree))
        fw.write("\n")

        for (bodytree <- body) {
          fw.write("bodytree:\n")
          fw.write(show(bodytree) + "\n")
          fw.write(formatAst(showRaw(bodytree)) + "\n")
          fw.write("\n")
        }

        fw.write("\n")
      }

      fw.close()

      chicalaLog.close()
    }
  }

}
