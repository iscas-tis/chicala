package chicala.ast

import scala.tools.nsc.Global

trait SStatements { self: ChicalaAst =>
  val global: Global
  import global._

  trait SStatementImpl { self: SStatement => }

  trait SDefDefImpl { self: SDefDef => }
  trait SValDefImpl { self: SValDef => }
  trait SApplyImpl { self: SApply =>
    override def toString(): String = {
      val name = this.getClass().getSimpleName()
      s"$name(${showRaw(appl)})"
    }
  }
  trait SSelectImpl { self: SSelect =>
    override def toString(): String = {
      val name = this.getClass().getSimpleName()
      s"$name(${showRaw(select)})"
    }
  }
  trait SBlockImpl { self: SBlock =>
    def isEmpty  = stats.isEmpty && expr.isEmpty
    def nonEmpty = !isEmpty
  }
}
