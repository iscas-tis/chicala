package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait STypeImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait StWrappedImpl { self: StWrapped =>
    override def toString(): String = s"StWrapped(\"${str}\")"
  }

  trait KnownSizeObjImpl {
    def fromInt(size: Int): KnownSize = {
      KnownSize(SLiteral(size, StInt))
    }
  }
}
