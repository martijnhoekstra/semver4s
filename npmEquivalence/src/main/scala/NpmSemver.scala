package semver4s.npm

import scala.scalajs.js
import js.annotation._

case class MatchOptions(loose: Boolean, includePreRelease: Boolean)
object MatchOptions:
  val strict  = MatchOptions(false, false)
  val lenient = MatchOptions(true, true)

@js.native
@JSImport("semver/index.js", JSImport.Namespace)
@annotation.nowarn("msg=parameter value")
object NPMSemver extends js.Object:

  /** returns identity if valid, or null if invalid */
  def valid(versionString: String, matchOptions: MatchOptions = MatchOptions.strict): String =
    js.native

  /** returns a valid semver string from an approximate version, or null if invalid */
  def clean(approximateString: String, matchOptions: MatchOptions = MatchOptions.strict): String =
    js.native

  /** returns true if the version string matches the range */
  def satisfies(
      versionString: String,
      rangeString: String,
      matchOptions: MatchOptions = MatchOptions.strict
  ): Boolean = js.native

  /** returns true if the first string represents a SemVer version that's greater than the semver
    * version represented by the second string
    */
  def gt(first: String, second: String, matchOptions: MatchOptions = MatchOptions.strict): Boolean =
    js.native

  /** returns true if the first string represents a SemVer version that's smaller than the semver
    * version represented by the second string
    */
  def lt(first: String, second: String, matchOptions: MatchOptions = MatchOptions.strict): Boolean =
    js.native

  /** returns the smallest version that matches the given range */
  def minVersion(rangeString: String, matchOptions: MatchOptions = MatchOptions.strict): String =
    js.native

  /** This aims to provide a very forgiving translation of a non-semver string to semver. It looks
    * for the first digit in a string, and consumes all remaining characters which satisfy at least
    * a partial semver (e.g., 1, 1.2, 1.2.3) up to the max permitted length (256 characters). Longer
    * versions are simply truncated (4.6.3.9.2-alpha2 becomes 4.6.3). All surrounding text is simply
    * ignored (v3.4 replaces v3.3.1 becomes 3.4.0). Only text which lacks digits will fail coercion
    * (version one is not valid). The maximum length for any semver component considered for
    * coercion is 16 characters
    */
  def coerce(approximate: String, matchOptions: MatchOptions = MatchOptions.strict): String =
    js.native
