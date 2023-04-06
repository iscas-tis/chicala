package chicala.convert

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.PluginComponent

import java.io._

import chicala.sort.StatementSortComponent
import chicala.util.Format

object ChiselToScalaComponent {
  val phaseName = "chiselToScala"
}

class ChiselToScalaComponent(val global: Global) extends PluginComponent {
  implicit private val implicitGlobal = global
  import global._

  private val fmt = new Format
  import fmt._

  val runsAfter: List[String] = List(StatementSortComponent.phaseName)
  // to keep recursive structure
  override val runsBefore: List[String] = List("tailcalls")

  val phaseName: String      = ChiselToScalaComponent.phaseName
  def newPhase(_prev: Phase) = new ChiselToScalaPhase(_prev)

  class ChiselToScalaPhase(prev: Phase) extends StdPhase(prev) {
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
          fw.write(showFormattedRaw(bodytree) + "\n")
          fw.write("\n")
        }

        fw.write("\n")
      }

      fw.close()

      chicalaLog.close()
    }
  }

}
