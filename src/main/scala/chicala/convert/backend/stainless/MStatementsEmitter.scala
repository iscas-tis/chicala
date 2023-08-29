package chicala.convert.backend.stainless

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.convert.backend.util._

trait MStatementsEmitter { self: StainlessEmitter with ChicalaAst =>
  val global: Global
  import global._

  trait MStatementEmitterImplicit { self: StainlessEmitterImplicit =>
    implicit class MStatementEmitter(mStatement: MStatement) {
      def toCodeLines: CodeLines = mStatement match {
        case x: MTerm => x.toCodeLines
        case x: MDef  => x.toCodeLines
      }
    }
  }
}
