package semver4s

import org.scalacheck.Prop.forAll

class NpmEquivalenceTest extends munit.ScalaCheckSuite {
  import semver4s.npm.NPMSemver
  import semver4s.GenMatcher._
  import semver4s.GenVersion._

  val maxSafeInt = 9007199254740991L

  test("NPM semver works") {
    assert(NPMSemver.satisfies("1.2.3", "1.x"))
  }

  property("Semver4s and NPM are consistent in what versions satisfy a range") {

    forAll(genRangeSet, genVersion)((rs, v) => {
      val m = matcher(rs).toOption.get
      //semver4s allows a bit more. Particularly, semver4s allows any numeric version part up to long
      //npm requires <= MaxSafeInteger
      def isJsSafeVersion(v: Version) = v.major <= maxSafeInt && v.minor <= maxSafeInt && v.patch <= maxSafeInt
      def isJsSafeMatcher(m: Matcher): Boolean = m match {
        case Matcher.And(simples) => simples.forall(isJsSafeMatcher)
        case Matcher.Or(ands) => ands.forall(isJsSafeMatcher)
        case Matcher.Hyphen(from, to) => isJsSafeVersion(from.version) && isJsSafeVersion(to.version)
        case Matcher.Caret(base) => isJsSafeVersion(base.version)
        case Matcher.Tilde(base) => isJsSafeVersion(base.version)
        case s: Matcher.Primitive => isJsSafeVersion(s.p.version)
      }
      if (isJsSafeVersion(v) && isJsSafeMatcher(m)){
        val versionString = v.format
        val semver4sMatches = m.matches(v)
        val npmMatches = NPMSemver.satisfies(clue(versionString), clue(rs))
        val explain = if (semver4sMatches) "semver4s matches, but npm doesn't"
                      else "npm matches, but semver4s doesn't"
        assertEquals(semver4sMatches, npmMatches, clue((rs, v, explain)))
      }
    })
  }

}