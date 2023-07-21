package chicala.util

import scala.tools.nsc.Global

trait ChiselAstCheck {
  val global: Global
  import global._

  def Chisel3Package(tree: Tree): Boolean   = tree.toString() == "chisel3.`package`"
  def Chisel3WhenApply(tree: Tree): Boolean = tree.toString() == "chisel3.when.apply"

  def ReturnAssert(tree: Tree): Boolean = tree.tpe.toString().endsWith(": chisel3.assert.Assert")

  def SourceInfoFun(fun: Tree): Boolean =
    fun.tpe.toString().startsWith("(implicit sourceInfo: chisel3.internal.sourceinfo.SourceInfo")

  /** pass through all unneed AST
    *
    * @param tree
    *   original tree
    * @return
    *   sub tree, optional typed info
    */
  def passThrough(tree: Tree): (Tree, Option[Tree]) = {
    tree match {
      case Typed(expr, tpt)                       => (passThrough(expr)._1, Some(tpt))
      case Apply(fun, args) if SourceInfoFun(fun) => passThrough(fun)
      case _                                      => (tree, None)
    }
  }
}
