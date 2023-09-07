package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst

trait ChicalaPeeks extends ChicalaPasss with Format { self: ChicalaAst =>
  val global: Global
  import global._

  class ChicalaPeek(path: String, tag: String) extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      Format.saveToFile(
        path + s"/${cClassDef.name}.chicala.${tag}.scala",
        cClassDef.toString
      )

      cClassDef
    }
  }

  object ChicalaPeek {
    def apply(path: String, tag: String): ChicalaPeek = new ChicalaPeek(path, tag)
  }
}
