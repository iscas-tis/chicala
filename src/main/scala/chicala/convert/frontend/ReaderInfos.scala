package chicala.convert.frontend

import scala.tools.nsc.Global

trait ReaderInfos { this: Scala2Reader =>
  val global: Global
  import global._

  case class ReaderInfo(
      moduleDefs: Map[String, ModuleDef],
      bundleDefs: Map[String, BundleDef],
      todos: List[(Tree, String)],
      dependentClassNotDef: Boolean
  ) {
    def settedDependentClassNotDef  = copy(dependentClassNotDef = true)
    def clearedDependentClassNotDef = copy(dependentClassNotDef = false)
    def isDependentClassNotDef      = dependentClassNotDef

    def addedModuleDef(moduleDef: ModuleDef) =
      copy(moduleDefs = moduleDefs + (moduleDef.fullName -> moduleDef))
    def addedBundleDef(bundleDef: BundleDef) =
      copy(bundleDefs = bundleDefs + (bundleDef.fullName -> bundleDef))
    def addedTodo(tree: Tree, packageName: String) = copy(todos = todos.appended((tree, packageName)))

    def needExit = isDependentClassNotDef
  }

  object ReaderInfo {
    def empty = ReaderInfo(Map.empty, Map.empty, List.empty, false)
  }

}
