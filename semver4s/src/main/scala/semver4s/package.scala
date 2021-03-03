/** Semver4s provides semver versions and matchers and parsers for them.
  *
  * SemVer is a version scheme to delineate when a libraries semantics change.
  * You can read more about it at https://semver.org/
  * 
  * This library aims to provide at least all functionality the SemVer package in NPM
  * provides, though as of now, it still falls short of that.
  * 
  * It does so through the types [[Matcher]] and [[Version]]. You can parse matcher
  * or version strings with [[semver4s.parseMatcher]] and [[semver4s.parserVersion]].
  * 
  * For supported ranges, see the NPM documentation over at https://www.npmjs.com/package/semver#ranges
  * 
  * For literals, you have the interpolators [[Literal.m]] and [[Literal.v]], so
  * you can write, for example a literal version `v"1.2.3"` or literal matcher
  * `m"~1.3.2 || 2.0.x - 2.2.x"`, which are parsed and checked at compile time.
  */
package object semver4s {
  import parsing._
  import cats.parse.Parser

  /** A parser for SemVer versions, like "1.2.3" or "2.12.3-rc1"
    */
  val semverParser: Parser[Version]  = SemverParser.semver
  /** A parser for SemVer matchers, like "~1.3.2"
    */
  val matcherParser: Parser[Matcher] = MatcherParser.rangeSet

  /** Parsers a string into a matcher, or an error with information where
    * the parse failed.
    * 
    * To match a literal string, use [[Literal.matcher]] or [[Literal.m]]
    */
  def parseMatcher(matcherString: String) = matcherParser.parseAll(matcherString)
  
  /** Parsers a string into a version, or an error with information where
    * the parse failed.
    * 
    * To match a literal string, use [[Literal.version]] or [[Literal.v]]
    */
  def parseVersion(versionString: String) = semverParser.parseAll(versionString)
}
