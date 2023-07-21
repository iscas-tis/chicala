package chicala.util

import scala.tools.nsc
import nsc.Global

import java.io._

trait Format {
  import Format._

  val global: Global
  import global._

  def showFormattedRaw(any: Any) = {
    formatAst(showRaw(any))
  }
  def showFormattedRaw(any: Any, level: Int) = {
    formatAst(showRaw(any), level)
  }
}

object Format {

  /** Format raw AST string.
    *
    * Get raw AST by `showRaw(tree)`
    *
    * @param s
    *   Raw AST string
    * @return
    *   Formatted string
    */
  def formatAst(s: String): String = {
    val indenter = "  "
    s.foldLeft((0, ""))((p, c) => {
      c match {
        case '(' => (p._1 + 1, p._2 + "(\n" + indenter * (p._1 + 1))
        case ')' => (p._1 - 1, p._2 + "\n" + indenter * (p._1 - 1) + ")")
        case ',' => (p._1, p._2 + ",\n" + indenter * p._1)
        case ' ' => p
        case _   => (p._1, p._2 + c)
      }
    })._2
  }

  def formatAst(s: String, level: Int): String = {
    val indenter = "  "
    val lv       = level
    s.foldLeft((0, "")) {
      case ((ind, past), c) => {
        c match {
          case '(' =>
            if (ind + 1 <= lv) (ind + 1, past + "(\n" + indenter * (ind + 1))
            else (ind + 1, past + "(")
          case ')' =>
            if (ind <= lv) (ind - 1, past + "\n" + indenter * (ind - 1) + ")")
            else (ind - 1, past + ")")
          case ',' =>
            if (ind <= lv) (ind, past + ",\n" + indenter * ind)
            else (ind, past + ",")
          case ' ' =>
            if (ind <= lv) (ind, past)
            else (ind, past + ' ')
          case _ => (ind, past + c)
        }
      }
    }._2
  }

  def saveToFile(path: String, s: String): Unit = {
    val cClassDefFile = new BufferedWriter(
      new PrintWriter(path)
    )
    cClassDefFile.write(s)
    cClassDefFile.close()
  }
}
