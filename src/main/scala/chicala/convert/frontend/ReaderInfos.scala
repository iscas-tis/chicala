package chicala.convert.frontend

import scala.tools.nsc.Global

trait ReaderInfos { this: Scala2Reader =>
  val global: Global
  import global._

  case class ReaderInfo(
      moduleDefs: Map[String, ModuleDef],
      bundleDefs: Map[String, BundleDef],
      todos: List[Tree],
      dependentClassNotDef: Boolean
  ) {
    def settedDependentClassNotDef  = copy(dependentClassNotDef = true)
    def clearedDependentClassNotDef = copy(dependentClassNotDef = false)
    def isDependentClassNotDef      = dependentClassNotDef

    def addedTodo(tree: Tree) = copy(todos = todos.appended(tree))

    def needExit = isDependentClassNotDef
  }

  object ReaderInfo {
    def empty = ReaderInfo(Map.empty, Map.empty, List.empty, false)
  }

}
