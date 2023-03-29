package chicala.util

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
}
