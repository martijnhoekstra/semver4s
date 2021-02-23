package semver4s.parsing

import cats.parse.Parser
import cats.data.NonEmptyList
import cats.parse.LocationMap
import cats.parse.Parser.Expectation._

/** A reporter for parse errors
  *
  * Eagerly allocates a LocationMap, which may be expensive: in most cases,
  * you want a def for a new reporter, and/or pass it as a by name argument.
  */
class Reporter(source: String) {
  val map = LocationMap(source)

  /** Gets an error report for the given parse error
    *
    * The report consists of a non-empty list of lines describing the error,
    * the first of which is the line on which the error occurred, and the second
    * is a caret pointing to the error position in the line.
    *
    * Following lines indicate what expectations were violated.
    */
  def report(error: Parser.Error): NonEmptyList[String] = {
    def pointer(offset: Int): List[String] = (for {
      (lineNumber, col) <- map.toLineCol(offset)
      line              <- map.getLine(lineNumber)
    } yield List(line, (" " * col) + "^")).getOrElse(Nil)

    val errors = error.expected
      .groupBy(_.offset)
      .map {
        case (offset, expectations) => {
          val printable = expectations.map(prettyExpectation).collect { case Some(text) => text }
          val reasons = (printable: @unchecked) match {
            case Nil         => Nil
            case head :: Nil => List(s"Expected $head")
            case head :: (init :+ last) =>
              (s"Expected $head" :: init).map(reason => s"$reason or") ::: List(last)
          }
          pointer(offset) ::: reasons
        }
      }
      .flatten

    NonEmptyList.fromListUnsafe(errors.toList)
  }

  /** Pretty-prints an expectation, if we know how to.
    *
    * Pretty-printing is done for OneOfStr, InRange, StartOfString, EndOfString,
    * Length and FailWith.
    */
  def prettyExpectation(exp: Parser.Expectation): Option[String] = {
    val pf: PartialFunction[Parser.Expectation, String] = {
      case OneOfStr(_, options) =>
        s"one of the following: " + options.map(str => s"'$str'").mkString(", ")
      case InRange(_, lower, upper) if (lower == upper) => s"character ${charInfo(lower)}"
      case InRange(_, lower, upper) =>
        s"a character between ${charInfo(lower)} and ${charInfo(upper)}"
      case StartOfString(_)            => "the start of input"
      case EndOfString(_, _)           => "the end of the string"
      case Length(_, expected, actual) => s"$expected remaining characters, but $actual found"
      case FailWith(_, message)        => message
    }
    pf.lift(exp)
  }

  /** Get a human-readable representation of some character.
    *
    * If the character is an ASCII printable character, itself.
    * Otherwise, include its name, Unicode codepoint, well-known escape sequence
    * as applicable.
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
      val name = Character.getName(ch.toInt)
      if (Character.isLetterOrDigit(ch)) s"'$ch': $name, codepoint $asU"
      else if (Character.isWhitespace(ch)) s"$name, codepoint $asU"
      else if (name.isEmpty) s"non-character $asU"
      else if (Character.isISOControl(ch)) s"control character $asU"
      else if (printable.contains(Character.getType(ch).toByte)) s"'$ch': $name, codepoint $asU"
      else s"$name, codepoint $asU"
    }
  }

}
