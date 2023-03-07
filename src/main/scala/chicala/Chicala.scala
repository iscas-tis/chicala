package chicala

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

import scala.reflect.internal.Trees

class Chicala(val global: Global) extends Plugin {
  import global._

  val name                              = "chicala"
  val description                       = "checks for division by zero"
  val components: List[PluginComponent] = List(Component)

  private object Component extends PluginComponent {
    val global: Chicala.this.global.type = Chicala.this.global

    val runsAfter: List[String] = List("refchecks")
    val phaseName: String       = Chicala.this.name

    def newPhase(_prev: Phase) = new DivByZeroPhase(_prev)

    class DivByZeroPhase(prev: Phase) extends StdPhase(prev) {
      override def name = Chicala.this.name

      def apply(unit: CompilationUnit): Unit = {
        for (
          tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body
          if rcvr.tpe <:< definitions.IntClass.tpe
        ) {
          global.reporter.error(tree.pos, "definitely division by zero")
        }
      }
    }
  }
}
