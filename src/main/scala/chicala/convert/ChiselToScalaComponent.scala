package chicala.convert

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.PluginComponent
import nsc.transform.TypingTransformers

import java.io._

import chicala.sort.StatementSortComponent
import chicala.util.Format
import chicala.ast.ChicalaAst

object ChiselToScalaComponent {
  val phaseName = "chiselToScala"
}

class ChiselToScalaComponent(val global: Global) extends PluginComponent with TypingTransformers {
  import global._

  val runsAfter: List[String]                 = List(StatementSortComponent.phaseName)
  override val runsRightAfter: Option[String] = Some(StatementSortComponent.phaseName)

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

  class ChiselToScalaTransformer(unit: CompilationUnit) extends TypingTransformer(unit) with ChicalaAst with Format {
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
        val classDefFile = new BufferedWriter(new PrintWriter(testRunDir.getPath() + s"/${packageName}.${name}.scala"))
        classDefFile.write(show(tree) + "\n")
        classDefFile.close()

        val classDefAstFile = new BufferedWriter(
          new PrintWriter(testRunDir.getPath() + s"/${packageName}.${name}.AST.scala")
        )
        classDefAstFile.write(showFormattedRaw(tree) + "\n")
        classDefAstFile.close()

        val cClassDef = CClassDef.fromTree(tree)

        val cClassDefFile = new BufferedWriter(
          new PrintWriter(testRunDir.getPath() + s"/${packageName}.${name}.chicala.scala")
        )
        cClassDefFile.write(Format.formatAst(cClassDef.toString) + "\n")
        cClassDefFile.close()

        tree // for now
      }
      case _ => super.transform(tree)
    }
  }

}
