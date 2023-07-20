package chicala

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

import chicala.convert.ChiselToScalaComponent

object ChicalaPlugin {
  val name: String        = "chicala"
  val description: String = "Convert Chisel to semantically equivalent Scala program"
}

class ChicalaPlugin(val global: Global) extends Plugin {
  import global._

  val name: String        = ChicalaPlugin.name
  val description: String = ChicalaPlugin.description

  val components: List[PluginComponent] = List(
    new ChiselToScalaComponent(global)
  )
}
