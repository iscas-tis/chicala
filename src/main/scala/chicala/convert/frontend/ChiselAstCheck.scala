package chicala.convert.frontend

import scala.tools.nsc.Global

trait ChiselAstCheck extends Utils { this: Scala2Reader =>
  val global: Global
  import global._

  def isChisel3Package(tree: Tree): Boolean = tree.toString() == "chisel3.`package`"

  def isChisel3WhenApply(tree: Tree): Boolean = tree.toString() == "chisel3.when.apply"

  def isChiselWireDefApply(tree: Tree): Boolean = List(
    isChisel3WireApply(_),
    isChisel3WireInitApply(_),
    isChisel3VecInitDoApply(_)
  ).exists(_(tree))
  def isChisel3WireApply(tree: Tree): Boolean =
    passThrough(tree)._1.toString() == "chisel3.Wire.apply"
  def isChisel3WireInitApply(tree: Tree): Boolean =
    passThrough(tree)._1.toString() == "chisel3.`package`.WireInit.apply"
  def isChisel3VecInitDoApply(tree: Tree): Boolean =
    passThrough(tree)._1.toString() == "chisel3.VecInit.do_apply"

  def isChiselRegDefApply(tree: Tree): Boolean = List(
    isChisel3RegApply(_),
    isChisel3RegInitApply(_),
    isChisel3UtilRegEnableApply(_)
  ).exists(_(tree))
  def isChisel3RegApply(tree: Tree): Boolean =
    passThrough(tree)._1.toString() == "chisel3.Reg.apply"
  def isChisel3RegInitApply(tree: Tree): Boolean =
    passThrough(tree)._1.toString() == "chisel3.RegInit.apply"
  def isChisel3UtilRegEnableApply(tree: Tree): Boolean =
    passThrough(tree)._1.toString() == "chisel3.util.RegEnable.apply"

  def isChisel3ModuleDoApply(tree: Tree): Boolean =
    passThrough(tree)._1.toString() == "chisel3.Module.do_apply"

  // Chisel Literal
  def isChiselLiteralType(tree: Tree): Boolean = List(
    isChisel3FromIntToLiteralType(_),
    isChisel3FromStringToLiteralType(_),
    isChisel3FromBooleanToLiteralType(_),
    isChisel3FromBigIntToLiteralType(_)
  ).exists(_(tree))
  def isChisel3FromIntToLiteralType(tree: Tree): Boolean = List(
    "chisel3.fromIntToLiteral",
    "chisel3.package.fromIntToLiteral"
  ).contains(tree.tpe.toString())
  def isChisel3FromStringToLiteralType(tree: Tree): Boolean = List(
    "chisel3.fromStringToLiteral",
    "chisel3.package.fromStringToLiteral"
  ).contains(tree.tpe.toString())
  def isChisel3FromBooleanToLiteralType(tree: Tree): Boolean = List(
    "chisel3.fromBooleanToLiteral",
    "chisel3.package.fromBooleanToLiteral"
  ).contains(tree.tpe.toString())
  def isChisel3FromBigIntToLiteralType(tree: Tree): Boolean = List(
    "chisel3.fromBigIntToLiteral",
    "chisel3.package.fromBigIntToLiteral"
  ).contains(tree.tpe.toString())

  def isChisel3UtilEnumApply(tree: Tree): Boolean = tree.toString() == "chisel3.util.Enum.apply"

  def isModuleThisIO(tree: Tree, cInfo: CircuitInfo): Boolean =
    passThrough(tree)._1.toString() == s"${cInfo.name}.this.IO"

  def isReturnAssert(tree: Tree): Boolean = tree.tpe.toString().endsWith(": chisel3.assert.Assert")

  def isSourceInfoFun(fun: Tree): Boolean =
    fun.tpe.toString().startsWith("(implicit sourceInfo: chisel3.internal.sourceinfo.SourceInfo")
  def isCompileOptionsFun(fun: Tree): Boolean =
    fun.tpe.toString().startsWith("(implicit compileOptions: chisel3.CompileOptions):")
  def isReflectClassTagFun(fun: Tree): Boolean =
    """\(implicit evidence.*: scala\.reflect\.ClassTag\[.*\]\): .*""".r.matches(fun.tpe.toString())

  def isChisel3EnumTmpValDef(tree: Tree): Boolean = tree match {
    case ValDef(mods, name, tpt, Match(Typed(Apply(cuea, number), _), _))
        if mods.isPrivate && mods.isLocalToThis && mods.isSynthetic &&
          mods.isArtifact && isChisel3UtilEnumApply(cuea) =>
      true
    case _ => false
  }

  def isChisel3UtilSwitchContextType(tree: Tree): Boolean =
    """chisel3\.util\.SwitchContext\[.*\]""".r.matches(tree.tpe.toString())

  def isChiselSignalType(tree: Tree): Boolean = {
    val tpe = autoTypeErasure(tree)

    List(
      "chisel3.UInt",
      "chisel3.SInt",
      "chisel3.Bool",
      "chisel3.Data"
    ).contains(tpe.toString()) || List(
      isChiselVecType(_),
      isChiselBundleType(_)
    ).exists(_(tree))

  }
  def isChiselVecType(tree: Tree): Boolean = {
    val tpe = autoTypeErasure(tree)
    """chisel3\.Vec\[.*\]""".r.matches(tpe.toString())
  }
  def isChiselBundleType(tree: Tree): Boolean = {
    val tpe = autoTypeErasure(tree)

    """chisel3\.Bundle\{.*\}""".r.matches(tpe.toString()) ||
    (tpe match {
      case tr @ TypeRef(pre, sym, args) =>
        if (sym.isClass)
          sym.asClass.baseClasses.map(_.toString()).contains("class Bundle")
        else
          false
      case _ => false
    })
  }

  def isChiselModuleType(tree: Tree): Boolean = {
    val tpe = autoTypeErasure(tree)

    tpe match {
      case tr @ TypeRef(pre, sym, args) =>
        if (sym.isClass)
          sym.asClass.baseClasses.map(_.toString()).contains("class Module")
        else
          false
      case _ => false
    }
  }

  def isScala2TupleType(tree: Tree): Boolean = tree.tpe.typeSymbol.fullName.startsWith("scala.Tuple")

  def isScala2TupleApply(tree: Tree): Boolean = """scala\.Tuple[0-9]*\.apply\[.*\]""".r.matches(tree.toString())

  def isScala2TupleUnapplyTmpValDef(tree: Tree): Boolean = tree match {
    case ValDef(mods, name, tpt, Match(Typed(Apply(appl, _), _), _)) if isScala2TupleApply(appl) => true
    case _                                                                                       => false
  }
}

trait Utils { self: ChiselAstCheck =>
  val global: Global
  import global._

  def autoTypeErasure(tr: Tree): Type = {
    if (tr.tpe.toString().endsWith(".type")) tr.tpe.erasure else tr.tpe
  }

  /** pass through all unneed AST
    *
    * @param tree
    *   original tree
    * @return
    *   sub tree, optional typed info
    */
  def passThrough(tree: Tree): (Tree, TypeTree) = tree match {
    case Typed(expr, tpt: TypeTree)                    => (passThrough(expr)._1, tpt)
    case Apply(fun, args) if isSourceInfoFun(fun)      => (passThrough(fun)._1, TypeTree(tree.tpe))
    case Apply(fun, args) if isCompileOptionsFun(fun)  => (passThrough(fun)._1, TypeTree(tree.tpe))
    case Apply(fun, args) if isReflectClassTagFun(fun) => passThrough(fun)
    case TypeApply(fun, args)                          => (passThrough(fun)._1, TypeTree(tree.tpe))
    case _                                             => (tree, TypeTree(tree.tpe))
  }

  def strippedName(name: TermName) = name.stripSuffix(" ")
}
