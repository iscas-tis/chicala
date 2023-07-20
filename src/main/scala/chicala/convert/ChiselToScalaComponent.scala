package chicala.convert

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.PluginComponent
import nsc.transform.TypingTransformers

import java.io._

import chicala.util.Format
import chicala.ast.ChicalaAst

object ChiselToScalaComponent {
  val phaseName = "chiselToScala"
}

class ChiselToScalaComponent(val global: Global) extends PluginComponent with TypingTransformers {
  import global._

  val runsAfter: List[String] = List("typer")

  // to keep recursive structure
  override val runsBefore: List[String] = List("tailcalls")

  val phaseName: String = ChiselToScalaComponent.phaseName

  def newPhase(_prev: Phase)                = new ChiselToScalaPhase(_prev)
  def newTransformer(unit: CompilationUnit) = new ChiselToScalaTransformer(unit)

  class ChiselToScalaPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      unit.body = newTransformer(unit).transform(unit.body)
    }
  }

  class ChiselToScalaTransformer(unit: CompilationUnit)
      extends TypingTransformer(unit)
      with ChicalaAst
      with ToplogicalSort
      with Format {
    lazy val global: ChiselToScalaComponent.this.global.type = ChiselToScalaComponent.this.global

    val testRunDir = new File("test_run_dir/" + phaseName)
    testRunDir.mkdirs()

    val chicalaLog = new BufferedWriter(new PrintWriter(testRunDir.getPath() + "/chicala_log.txt"))
    global.computePhaseAssembly().foreach(s => chicalaLog.write(s.toString + "\n"))
    chicalaLog.close()

    val packageDef  = unit.body.asInstanceOf[PackageDef]
    val packageName = packageDef.pid.toString()

    override def transform(tree: Tree): Tree = tree match {
      case ClassDef(mods, name, tparams, Template(parents, self, body)) => {
        Format.saveToFile(
          testRunDir.getPath() + s"/${packageName}.${name}.scala",
          show(tree) + "\n"
        )
        Format.saveToFile(
          testRunDir.getPath() + s"/${packageName}.${name}.AST.scala",
          showFormattedRaw(tree) + "\n"
        )

        val cClassDef = CClassDef.fromTree(tree)

        Format.saveToFile(
          testRunDir.getPath() + s"/${packageName}.${name}.chicala.scala",
          Format.formatAst(cClassDef.toString) + "\n"
        )

        val sortedCClassDef = cClassDef match {
          case Some(m @ ModuleDef(name, info, body)) =>
            Format.saveToFile(
              testRunDir.getPath() + s"/${packageName}.${name}.related.scala",
              body.map(s => s.toString() + "\n" + s.relatedSignals + "\n").fold("")(_ + _)
            )
            val sorted = Some(dependencySort(m))
            Format.saveToFile(
              testRunDir.getPath() + s"/${packageName}.${name}.sorted.scala",
              sorted.get.toString
            )
            sorted
          case Some(BundleDef(_, _)) => cClassDef
          case None                  => None
        }

        tree // for now
      }
      case _ => super.transform(tree)
    }
  }

}
