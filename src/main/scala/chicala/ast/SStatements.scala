package chicala.ast

import scala.tools.nsc.Global

trait SStatements { self: CStatements =>
  val global: Global
  import global._

  trait SStatementImpl { self: SStatement =>
    val tree: Tree

    override def toString(): String = {
      val name = this.getClass().getSimpleName()
      val ast  = showRaw(tree).replace("\"", "\\\"")
      s"$name(\"$ast\")"
    }
  }

  trait SDefDefImpl { self: SDefDef => }
  trait SValDefImpl { self: SValDef => }
  trait SApplyImpl  { self: SApply =>  }
  trait SSelectImpl { self: SSelect => }
  trait SBlockImpl  { self: SBlock =>  }

}
