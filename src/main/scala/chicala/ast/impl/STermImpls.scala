package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait STermImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait SForImpl { self: SFor =>
    val tpe: MType = EmptyMType
  }

}
