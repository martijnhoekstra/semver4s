package semver4s

import org.scalacheck.Prop.forAll
import semver4s.JsSafeGenerators._
import semver4s.npm.NPMSemver

class NpmEquivalenceTestFewChecks extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters = super.scalaCheckTestParameters.withMinSuccessfulTests {
    if (sys.env.contains("CI")) 1 else 10
  }

  /* this test is too damn slow (50s-100s with 10 successful)
     I think it's the generators that retryUntil <255 chars */
  property("Semver4s and NPM are consistent in what versions satisfy a range") {
    import Shrinkers._
    implicit val stringShrinker = shrinkMatcherString
    forAll(jsSafeMatcher, jsSafeVersion)((rs, v) => {
      val m = parseMatcher(rs).toOption.get
      //semver4s allows a bit more. Particularly, semver4s allows any numeric version part up to long
      //npm requires <= MaxSafeInteger
      val semver4sMatches = m.matches(v)
      val npmMatches      = NPMSemver.satisfies(v.format, rs)
      val explain =
        if (semver4sMatches) "semver4s matches, but npm doesn't"
        else if (npmMatches) "npm matches, but semver4s doesn't"
        else "neither matches"
      assertEquals(npmMatches, semver4sMatches, clue((rs, v.format, explain)))
    })
  }

}
