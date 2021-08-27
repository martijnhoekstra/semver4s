package semver4s

/** The Semver4s core package provides zero-dependecies core semver functionality
  *
  * SemVer is a version scheme to delineate when a libraries semantics change. You can read more
  * about it at https://semver.org/
  *
  * This library aims to provide at least all functionality the SemVer package in NPM provides,
  * though as of now, it still falls short of that.
  *
  * It does so through the types [[Matcher]] and [[Version]]. You can parse matcher or version
  * strings with [[semver4s.core.parseMatcher]] and [[semver4s.core.parseVersion]].
  *
  * For supported ranges, see the NPM documentation over at
  * https://www.npmjs.com/package/semver#ranges
  */
package object core {

  /** Parsers a string into a matcher
    */
  def parseMatcher(matcherString: String): Either[String, Matcher] =
    coreparse.MatcherParser.parseMatcher(matcherString)

  /** Parsers a string into a version
    */
  def parseVersion(versionString: String): Either[String, Version] =
    coreparse.SemverParser.parseVersion(versionString)

}
