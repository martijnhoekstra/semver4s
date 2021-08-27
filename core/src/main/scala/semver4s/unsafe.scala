package semver4s

/** provides unsafe parsing, throwing exceptions for bad strings.
  *
  * this is useful for example in sbt builds, where you just want a version and don't mind crashing
  * if it's set to something bad
  */
object unsafe {

  /** get the version from a string, or throw an exception if it has a bad format
    */
  def version(versionString: String): Version = coreparse.SemverParser
    .parseVersion(versionString)
    .fold(
      err => throw new IllegalArgumentException(s"invalid version: $err"),
      identity
    )

  /** get a matcher from a string, or throw an exception if it has a bad format
    */
  def matcher(matcherString: String): Matcher = coreparse.MatcherParser
    .parseMatcher(matcherString)
    .fold(
      err => throw new IllegalArgumentException(s"invalid matcher: $err"),
      identity
    )

  implicit final class UnsafeVersionOps(val versionString: String) extends AnyVal {

    /** Checks wheter the receiver string, when interpreted as a semver version matches the given
      * string interpreted as a semver range
      *
      * @example
      *   "1.2.3".matchesRange(">=1 <2") //true
      * @example
      *   "something".matchesRange(">=1 <2") //IllegalArgumentException
      */
    def matchesRange(rangeString: String) = matcher(rangeString).matches(version(versionString))

    /** Checks wheter the receiver string, when interpreted as a semver version matches the given
      * string interpreted as a semver range
      *
      * @example
      *   "1.2.3".matchesRange(">=1 <2") //true
      * @example
      *   "something".satisfies(">=1 <2") //IllegalArgumentException
      */
    def satisfies(rangeString: String) = matchesRange(rangeString)

  }

  implicit final class UnsafeMatcherOps(val rangeString: String) extends AnyVal {

    /** Checks whether the receiver string, when interpreted as a semver matcher, matches the
      * argument string interpreted as a semver version
      *
      * @example
      *   ">=1 <2".matchesVersion("1.2.3")
      */
    def matchesVersion(versionString: String) = matcher(rangeString).matches(version(versionString))
  }
}
