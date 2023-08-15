package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.MTypes

trait STypeImpls { self: MTypes =>
  val global: Global
  import global._

  trait StWrappedImpl { self: StWrapped =>
    override def toString(): String = s"StWrapped(\"${str}\")"
  }
}
