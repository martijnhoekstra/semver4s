package semver4s

/** provides unsafe parsing, throwing exceptions for bad strings.
  *
  * this is useful for example in sbt builds, where you just want a version and don't mind crashing
  * if it's set to something bad
  */
object unsafe {
  //import parsing._
  //import cats.parse.Parser

  /** get the version from a string, or throw an exception if it has a bad format
    */
  def version(versionString: String) = parseVersion(versionString).fold(_ => ???, identity)

  /** get a matcher from a string, or throw an exception if it has a bad format
    */
  def matcher(matcherString: String) = parseMatcher(matcherString).fold(_ => ???, identity)

  implicit final class UnsafeVersionOps(val versionString: String) extends AnyVal {
    def matchesRange(rangeString: String) = matcher(rangeString).matches(version(versionString))
  }

  implicit final class UnsafeMatcherOps(val rangeString: String) extends AnyVal {
    def matchesVersion(versionString: String) = matcher(rangeString).matches(version(versionString))
  }
}
