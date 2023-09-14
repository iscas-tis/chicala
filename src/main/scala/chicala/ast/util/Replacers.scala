package chicala.ast.util

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait Replacers extends Transformers { self: ChicalaAst =>
  val global: Global
  import global._

  class Replacer(replaceMap: Map[MStatement, MStatement]) extends Transformer {
    val rMap = replaceMap.map({ case (k, v) => (k.toString() -> v) })

    override def transform(mStatement: MStatement): MStatement = {
      rMap.get(mStatement.toString()) match {
        case Some(value) => value
        case None        => super.transform(mStatement)
      }
    }
  }
  object Replacer {
    def apply(replaceMap: Map[MStatement, MStatement]): Replacer = {
      new Replacer(replaceMap)
    }
  }
}
