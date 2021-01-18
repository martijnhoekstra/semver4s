package semver4s.cli

import cats.parse.Parser
import cats.data.NonEmptyList
import cats.parse.LocationMap
import cats.parse.Parser.Expectation._

object Reporter {

  def report(error: Parser.Error, source: String): NonEmptyList[String] = {
    val map = LocationMap(source)
    def pointer(offset: Int): List[String] = (for {
      (linei, col) <- map.toLineCol(offset)
      line         <- map.getLine(linei)
    } yield List(line, (" " * col) + "^")).getOrElse(Nil)

    val errors = error.expected
      .groupBy(exp => exp.offset)
      .map {
        case (offset, expectations) => {
          val printable = expectations
            .map(prettyExpectation)
            .collect { case Some(text) => text }
            .toList
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

  def prettyExpectation(exp: Parser.Expectation) = {
    val pf: PartialFunction[Parser.Expectation, String] = {
      case OneOfStr(_, strs) =>
        s"one of the following: " + strs.map(str => s"'$str'").mkString(", ")
      case InRange(_, lower, upper) if (lower == upper) => s"character ${charinfo(lower)}"
      case InRange(_, lower, upper) =>
        s"a character between ${charinfo(lower)} and ${charinfo(upper)}"
      case StartOfString(_)            => "the start of input"
      case EndOfString(_, _)           => "the end of the string"
      case Length(_, expected, actual) => s"$expected remaining characters, but $actual found"
      case FailWith(_, message)        => message
    }
    pf.lift(exp)
  }

  def charinfo(ch: Char): String = {
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
      else if (name.isEmpty) s"noncharacter $asU"
      else if (Character.isISOControl(ch)) s"Control character $asU"
      else if (printable.contains(Character.getType(ch).toByte)) s"'$ch': $name, codepoint $asU"
      else s"$name, codepoint $asU"
    }
  }

}
