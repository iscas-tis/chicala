package chicala.convert

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.PluginComponent

import java.io._

import chicala.util.Format
import chicala.convert.frontend.Scala2Reader
import chicala.util.Printer
import chicala.convert.backend.stainless.StainlessEmitter

object ChiselToScalaComponent {
  val phaseName = "chiselToScala"
}

class ChiselToScalaComponent(val global: Global) extends PluginComponent {
  import global._

  val runsAfter: List[String] = List("typer")

  // to keep recursive structure
  override val runsBefore: List[String] = List("tailcalls")

  val phaseName: String = ChiselToScalaComponent.phaseName

  def newPhase(_prev: Phase) = new ChiselToScalaPhase(_prev)

  class ChiselToScalaPhase(prev: Phase)
      extends StdPhase(prev)
      with Scala2Reader
      with ToplogicalSort
      with StainlessEmitter
      with Format {
    lazy val global: ChiselToScalaComponent.this.global.type = ChiselToScalaComponent.this.global

    val testRunDir = new File("test_run_dir/" + phaseName)
    testRunDir.mkdirs()

    val chicalaLog = new BufferedWriter(new PrintWriter(testRunDir.getPath() + "/chicala_log.txt"))
    global.computePhaseAssembly().foreach(s => chicalaLog.write(s.toString + "\n"))
    chicalaLog.close()

    var readerInfo: ReaderInfo = ReaderInfo.empty

    override def run(): Unit = {
      super.run()
      processTodos()
      readerInfo.todos.foreach { case (t, pname) => reporter.error(t.pos, "This class not processed") }
    }

    def apply(unit: CompilationUnit): Unit = {
      val packageDef  = unit.body.asInstanceOf[PackageDef]
      val packageName = packageDef.pid.toString()

      for (tree @ ClassDef(mods, name, tparams, Template(parents, self, body)) <- packageDef.stats) {
        applyOnTree(tree, packageName)
      }
    }

    def applyOnTree(tr: Tree, packageName: String): Unit = {
      val packageDir = s"${testRunDir.getPath()}/test/${packageName.replace(".", "/")}"
      val outputDir  = s"${testRunDir.getPath()}/out/${packageName.replace(".", "/")}"
      (new File(packageDir)).mkdirs()
      (new File(outputDir)).mkdirs()

      tr match {
        case tree @ ClassDef(mods, name, tparams, Template(parents, self, body)) =>
          Format.saveToFile(
            packageDir + s"/${name}.scala",
            show(tree) + "\n"
          )
          Format.saveToFile(
            packageDir + s"/${name}.AST.scala",
            showFormattedRaw(tree) + "\n"
          )

          val someInfoAndDef = CClassDefLoader(tree, packageName)(readerInfo)

          val (newRInfo, someCClassDef) = someInfoAndDef match {
            case Some((newRInfo, someCClassDef)) => { (newRInfo, someCClassDef) }
            case None =>
              reporter.error(tree.pos, "Unknown error in ChiselToScalaPhase #1")
              return
          }

          if (newRInfo.needExit) {
            if (newRInfo.isDependentClassNotDef) {
              readerInfo = newRInfo.clearedDependentClassNotDef.addedTodo(tree, packageName)
            }
            if (readerInfo.needExit == true)
              reporter.error(tree.pos, "Unknown error in ChiselToScalaPhase #2")
            return
          }

          someCClassDef match {
            case None =>
              reporter.error(tree.pos, "Unknown error in ChiselToScalaPhase #3")
            case Some(cClassDef) =>
              Format.saveToFile(
                packageDir + s"/${name}.chicala.scala",
                cClassDef.toString + "\n"
              )

              val sortedCClassDef = cClassDef match {
                case m @ ModuleDef(name, info, body, pkg) =>
                  readerInfo = readerInfo.addedModuleDef(m)
                  Format.saveToFile(
                    packageDir + s"/${name}.related.scala",
                    body.map(s => s.toString() + "\n" + s.relatedSignals + "\n").fold("")(_ + _)
                  )
                  val sorted = dependencySort(m)
                  Format.saveToFile(
                    packageDir + s"/${name}.sorted.scala",
                    sorted.toString
                  )
                  sorted
                case b: BundleDef =>
                  readerInfo = readerInfo.addedBundleDef(b)
                  b
              }

              sortedCClassDef match {
                case m: ModuleDef =>
                  Format.saveToFile(
                    outputDir + s"/${name}.stainless.scala",
                    EmitStainless(m)
                  )
                case _ =>
              }

          }

      }
    }

    def processTodos(): Unit = {
      var lastNum = readerInfo.todos.size + 1
      while (readerInfo.todos.size > 0 && lastNum > readerInfo.todos.size) {
        val todos = readerInfo.todos
        lastNum = todos.size
        readerInfo = readerInfo.copy(todos = List.empty)
        todos.foreach { case (t, pname) => applyOnTree(t, pname) }
      }
    }

  }

}
