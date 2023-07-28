package chicala.convert.frontend

import scala.tools.nsc.Global

trait ChiselAstCheck { this: Scala2Loader =>
  val global: Global
  import global._

  def isChisel3Package(tree: Tree): Boolean   = tree.toString() == "chisel3.`package`"
  def isChisel3WhenApply(tree: Tree): Boolean = tree.toString() == "chisel3.when.apply"
  def isChisel3WireApply(tree: Tree): Boolean = passThrough(tree)._1.toString() == "chisel3.Wire.apply"

  def isModuleThisIO(tree: Tree, cInfo: CircuitInfo): Boolean =
    passThrough(tree)._1.toString() == s"${cInfo.name}.this.IO"

  def isReturnAssert(tree: Tree): Boolean = tree.tpe.toString().endsWith(": chisel3.assert.Assert")

  def isSourceInfoFun(fun: Tree): Boolean =
    fun.tpe.toString().startsWith("(implicit sourceInfo: chisel3.internal.sourceinfo.SourceInfo")
  def isCompileOptionsFun(fun: Tree): Boolean =
    fun.tpe.toString().startsWith("(implicit compileOptions: chisel3.CompileOptions):")

  def isChiselType(tree: Tree): Boolean = List(
    "chisel3.UInt",
    "chisel3.SInt",
    "chisel3.Bool",
    "chisel3.Bundle"
  ).contains(tree.tpe.erasure.toString())

  /** pass through all unneed AST
    *
    * @param tree
    *   original tree
    * @return
    *   sub tree, optional typed info
    */
  def passThrough(tree: Tree): (Tree, Option[Tree]) = {
    tree match {
      case Typed(expr, tpt)                             => (passThrough(expr)._1, Some(tpt))
      case Apply(fun, args) if isSourceInfoFun(fun)     => passThrough(fun)
      case Apply(fun, args) if isCompileOptionsFun(fun) => passThrough(fun)
      case TypeApply(fun, args)                         => (passThrough(fun)._1, Some(tree))
      case _                                            => (tree, None)
    }
  }
}
