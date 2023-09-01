package chicala.convert.backend.util

case class CodeLines(lines: List[String]) {
  def toCode: String = lines.mkString("\n")

  def indented: CodeLines = indented(1)
  def indented(indent: Int): CodeLines = {
    val prefix = "  " * indent
    CodeLines(
      lines.map(x =>
        if (x.isBlank()) x
        else prefix + x
      )
    )
  }

  def enddedWithExceptLast(ends: String): CodeLines = {
    lines match {
      case Nil => CodeLines.empty
      case _ =>
        val rev = lines.reverse
        CodeLines((rev.head :: rev.tail.map(_ + ends)).reverse)
    }
  }

  def concatLastLine(that: CodeLines): CodeLines = {
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else
      CodeLines(
        this.lines.init ++
          List(this.lines.last + that.lines.head) ++
          that.lines.tail
      )
  }

  /** If lines only has one line, wrap to one line */
  def wrappedToOneLineBy(left: CodeLines, right: CodeLines): CodeLines =
    lines match {
      case Nil         => left.concatLastLine(right)
      case head :: Nil => left.concatLastLine(CodeLines(head.strip)).concatLastLine(right)
      case _           => left ++ this ++ right
    }

  /** If lines only has one line, wrap to new line */
  def wrappedToNewLineBy(left: CodeLines, right: CodeLines): CodeLines =
    lines match {
      case Nil => left.concatLastLine(right)
      case _   => left ++ this ++ right
    }

  def ++(that: CodeLines): CodeLines = CodeLines(this.lines ++ that.lines)

  /** Append code string to back */
  def :+(thatStr: String): CodeLines = this ++ CodeLines(thatStr)

  /** Append code string to front */
  def +:(thatStr: String): CodeLines = CodeLines(thatStr) ++ this

  def isEmpty  = lines.isEmpty
  def nonEmpty = lines.nonEmpty
}

object CodeLines {
  def apply(code: String): CodeLines = CodeLines(code.split("\n").toList)
  def apply(head: CodeLines, tail: CodeLines*): CodeLines = {
    (head +: tail.toList).reduce(_ ++ _)
  }

  def warpToOneLine(left: CodeLines, mid: CodeLines, right: CodeLines) = {
    mid.wrappedToOneLineBy(left, right)
  }
  def warpToNewLine(left: CodeLines, mid: CodeLines, right: CodeLines) = {
    mid.wrappedToNewLineBy(left, right)
  }

  def empty = CodeLines(List.empty)
  def blank = CodeLines(List(""))
}

trait CodeLinesImplicit {
  import scala.language.implicitConversions

  implicit class StringListToCodeLines(list: List[String]) {
    def toCodeLines = CodeLines(list)
  }
  implicit class CodeLinesListToCodeLines(list: List[CodeLines]) {
    def toCodeLines = list.foldLeft(CodeLines.empty)(_ ++ _)
  }
  implicit def stringToCodeLines(str: String): CodeLines = {
    CodeLines(str)
  }
}
