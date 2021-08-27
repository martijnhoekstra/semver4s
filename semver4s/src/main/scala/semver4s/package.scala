/** Semver4s provides additional functionality over the core package
  *
  * It provides literals through [[Literal]], importing Literal._ brings the interpolators m and v
  * in scope, which you can use to write, for example a literal version `v"1.2.3"` or literal
  * matcher `m"~1.3.2 || 2.0.x - 2.2.x"`, which are parsed and validated at compile time.
  *
  * It also provides cats-parse based parsers with better error reporting, composibility and faster
  * performance.
  */
package object semver4s {
  import cats.parse.Parser

  /** A parser for SemVer versions, like "1.2.3" or "2.12.3-rc1"
    */
  val semverParser: Parser[Version] = _root_.semver4s.parsing.SemverParser.semver

  /** A parser for SemVer matchers, like "~1.3.2"
    */
  val matcherParser: Parser[Matcher] = _root_.semver4s.parsing.MatcherParser.rangeSet

  /** Parsers a string into a matcher, or an error with information where the parse failed.
    *
    * To match a literal string, use [[Literal.matcher]] or the m interpolator
    */
  def parseMatcher(matcherString: String): Either[Parser.Error, Matcher] =
    matcherParser.parseAll(matcherString)

  /** Parsers a string into a version, or an error with information where the parse failed.
    *
    * To match a literal string, use [[Literal.version]] or the v interpolator
    */
  def parseVersion(versionString: String): Either[Parser.Error, Version] =
    semverParser.parseAll(versionString)
}
