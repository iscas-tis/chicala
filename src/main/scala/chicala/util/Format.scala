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
    AstStr.format(s)
  }

  def saveToFile(path: String, s: String): Unit = {
    val cClassDefFile = new BufferedWriter(
      new PrintWriter(path)
    )
    cClassDefFile.write(s)
    cClassDefFile.close()
  }
}

case class AstStr(name: String, body: Option[List[AstStr]]) {
  def formatLines: List[String] = {
    body match {
      case None => List(name)
      case Some(ls) =>
        ls match {
          case Nil => List(name + "()")
          case head :: next => {
            val lines = ls
              .map(_.formatLines)
              .reduce({ (a, b) =>
                val ra = a.reverse
                ((ra.head + ",") :: ra.tail).reverse ++ b
              })
            if (lines.size == 1)
              List(s"${name}(${lines.head})")
            else if (name == "Select" && ls.size == lines.size)
              List(s"${name}(${lines.reduce(_ + " " + _)})")
            else List(name + "(") ++ lines.map("  " + _) ++ List(")")
          }
        }
    }
  }
  def format: String = {
    formatLines.reduce(_ + "\n" + _)
  }
}
object AstStr {
  def format(ast: String): String =
    AstStr(ast).format

  def getBody(ast: String): (Option[List[AstStr]], String) = {
    var rest = skip(ast)
    if (rest.isEmpty || ",|)".contains(rest.head))
      return (None, rest)

    var body = List.empty[AstStr]
    assert(rest.head == '(', s"what is this ${rest.head}")
    rest = rest.tail
    while (rest.nonEmpty && rest.head != ')') {
      val (newAst, newRest) = getAstStrPrefix(rest)
      body = body :+ newAst
      rest = skip(newRest)
      if (",|".contains(rest.head))
        rest = rest.tail
    }
    rest = rest.tail
    (Some(body), rest)
  }
  def findEnd(ast: String): Int = {
    def find(s: String, c: Char): Int = {
      val i = s.indexOf(c)
      if (i == -1) s.size else i
    }
    find(ast, '(')
      .min(find(ast, ')'))
      .min(find(ast, ','))
      .min(find(ast, ' '))
      .min(ast.size)
  }
  def skip(ast: String): String = {
    var rest = ast
    while (rest.nonEmpty && rest.head == ' ') rest = rest.tail
    rest
  }
  def getAstStrPrefix(ast: String): (AstStr, String) = {
    var rest = skip(ast)
    if (rest.head == '"') {
      val rq = rest.tail.indexOf('"') + 1
      return (AstStr(rest.take(rq + 1), None), rest.drop(rq + 1))
    }
    val end       = findEnd(rest)
    val name      = rest.take(end)
    var (body, r) = getBody(rest.drop(end))
    (AstStr(name, body), r)
  }
  def apply(ast: String): AstStr = {
    getAstStrPrefix(ast)._1
  }
}
