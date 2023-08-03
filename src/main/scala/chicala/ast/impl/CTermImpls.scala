package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.MStatements

trait CTermImpls { self: MStatements =>
  val global: Global
  import global._

}
