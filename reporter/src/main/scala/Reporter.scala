package catsparse.reporting

import cats.parse.Parser
import cats.data.NonEmptyList
import cats.parse.LocationMap
import cats.parse.Parser.Expectation._

/** A reporter for parse errors
  *
  * Eagerly allocates a LocationMap, which may be expensive: in most cases, you want a def for a new
  * reporter, and/or pass it as a by name argument.
  */
class Reporter(source: String) {
  val map = LocationMap(source)

  /** Gets an error report for the given parse error
    *
    * The report consists of a non-empty list of lines describing the error, the first of which is
    * the line on which the error occurred, and the second is a caret pointing to the error position
    * in the line.
    *
    * Following lines indicate what expectations were violated.
    */
  def report(error: Parser.Error, width: Int = 60): NonEmptyList[ErrorReport] = {
    def pointer(offset: Int, maxWidth: Int): Option[(String, Int)] = for {
      (lineNumber, col) <- map.toLineCol(offset)
      line              <- map.getLine(lineNumber)
    } yield {
      //for pretty printing, if the whole line doesn't fit
      //in the given width, we aim at having 75% of the line before the caret
      //position, and the rest after, so that the caret is on 3/4 of the line
      val startIndex =
        if (line.length <= maxWidth) 0
        else {
          val leadingTarget = (0.75 * maxWidth).toInt
          val leading       = math.min(col, leadingTarget)
          col - leading
        }
      val lineSegment = line.drop(startIndex).take(maxWidth)
      val p           = col - startIndex
      (lineSegment, p)
    }

    val errors = error.expected
      .groupBy(_.offset)
      .map {
        case (offset, expectations) => {
          val printable = expectations.map(prettyExpectation)
          pointer(offset, width) match {
            case Some((context, options)) => ErrorReport(context, options, printable)
            case None =>
              throw new IllegalArgumentException(
                "bad offset for position. Is the error from the same source as passed to the reporter?"
              )
          }
        }
      }

    NonEmptyList.fromListUnsafe(errors.toList)
  }

  /** Pretty-prints an expectation
    */
  def prettyExpectation(exp: Parser.Expectation): String =
    exp match {
      case OneOfStr(_, List(opt)) => s""""$opt""""
      case OneOfStr(_, options) =>
        s"one of the following: " + options.map(str => s""""$str"""").mkString(", ")
      case InRange(_, lower, upper) if (lower == upper) => s"character ${charInfo(lower)}"
      case InRange(_, lower, upper) if (lower + 1 == upper) =>
        s"character ${charInfo(lower)} or ${charInfo(upper)}"
      case InRange(_, lower, upper) =>
        s"a character between ${charInfo(lower)} and ${charInfo(upper)}"
      case StartOfString(_)             => "the start of input"
      case EndOfString(_, _)            => "the end of the string"
      case Length(_, expected, actual)  => s"$expected remaining characters, but $actual found"
      case FailWith(_, message)         => message
      case Fail(_)                      => "to fail"
      case ExpectedFailureAt(_, _)      => "to fail"
      case WithContext(context, expect) => s"${prettyExpectation(expect)} ($context)"
    }

  /** Get a human-readable representation of some character.
    *
    * If the character is an ASCII printable character, itself. Otherwise, include its name, Unicode
    * codepoint, well-known escape sequence as applicable.
    */
  def charInfo(ch: Char): String = {
    val asU = "\\u" + ch.toInt.toHexString
    val printable = Set(
      Character.CURRENCY_SYMBOL,
      Character.MATH_SYMBOL,
      Character.OTHER_SYMBOL,
      Character.CONNECTOR_PUNCTUATION,
      Character.DASH_PUNCTUATION,
      Character.END_PUNCTUATION,
      Character.FINAL_QUOTE_PUNCTUATION,
      Character.INITIAL_QUOTE_PUNCTUATION,
      Character.OTHER_PUNCTUATION,
      Character.START_PUNCTUATION
    )
    if (ch >= ' ' && ch <= '~') s"'$ch'"
    else if (ch == '\n') "'\\n'"
    else if (ch == '\r') "'\\r'"
    else if (ch == '\t') "'\\t'"
    else if (Character.isLowSurrogate(ch)) s"low surrogate $asU"
    else if (Character.isHighSurrogate(ch)) s"high surrogate $asU"
    else {
      //val name = Character.getName(ch.toInt) doesn't exist on JS
      if (Character.isLetter(ch)) s"letter '$ch', codepoint $asU"
      else if (Character.isLetterOrDigit(ch)) s"'$ch': codepoint $asU"
      else if (Character.isWhitespace(ch)) s"whitespace character, codepoint $asU"
      //else if (name.isEmpty) s"non-character $asU"
      else if (Character.isISOControl(ch)) s"control character $asU"
      else if (printable.contains(Character.getType(ch).toByte)) s"'$ch': codepoint $asU"
      else s"codepoint $asU"
    }
  }

}
