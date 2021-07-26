package semver4s

import org.scalacheck.Prop.forAll
import semver4s.npm.NPMSemver
import semver4s.Shrinkers._
import semver4s.gen.JsSafeGenerators._
import semver4s.gen.GenVersion._
import math.Ordering.Implicits.infixOrderingOps

class NpmEquivalenceTest extends munit.ScalaCheckSuite:

  test("NPM semver satisfies works") {
    assert(NPMSemver.satisfies("1.2.3", "1.x"))
    assert(!NPMSemver.satisfies("1.2.3", "2.x"))
  }

  def npmValid(str: String, clue: => Any = "") = assert(
    NPMSemver.valid(str) != null,
    if (clue == "") s"$str should be valid, but isn't" else clue
  )
  def npmInvalid(str: String, clue: => Any = "") = assert(
    NPMSemver.valid(str) eq null,
    if (clue == "") s"$str should be invalid, but is valid" else clue
  )

  test("NPM valid accepts example strings") {
    npmValid("1.2.3")
    npmValid("1.2.3-pre-1232.12-beta")
    npmInvalid("some random stuff")
    npmInvalid("")
    npmInvalid("90071992547409910.0.0", "major version > max safe int")
    npmInvalid("1.2.3-" + ("x" * 255), "version string too long")
  }

  property("Only jsSafeVersion's parse in NPM") {
    forAll(genVersion)(v => {
      val isNPMValid = NPMSemver.valid(v.format) != null
      assertEquals(isJsSafeVersion(v), isNPMValid, v.format)
    })
  }

  test("NPM gt examples") {
    assert(NPMSemver.gt(clue("1.2.3"), clue("1.2.2")))
    assert(NPMSemver.gt(clue("1.2.3"), clue("1.2.3-RC1")))
    assert(NPMSemver.gt(clue("1.2.3-RC1"), clue("1.2.2")))
    assert(NPMSemver.gt(clue("1.2.3-RC1"), clue("1.2.2-RC1")))
    assert(NPMSemver.gt(clue("1.2.3-RC2"), clue("1.2.2-RC1")))
    assert(NPMSemver.gt(clue("1.2.3-12"), clue("1.2.2-3")))
  }

  test("NPM lt examples") {
    assert(NPMSemver.lt(clue("1.2.2"), clue("1.2.3")))
    assert(NPMSemver.lt(clue("1.2.3-RC1"), clue("1.2.3")))
    assert(NPMSemver.lt(clue("1.2.2"), clue("1.2.3-RC1")))
    assert(NPMSemver.lt(clue("1.2.2-RC1"), clue("1.2.3-RC1")))
    assert(NPMSemver.lt(clue("1.2.2-RC1"), clue("1.2.3-RC2")))
    assert(NPMSemver.lt(clue("1.2.2-3"), clue("1.2.3-12")))
  }

  test("NPM long numeric suffixes are accepted and compared numerically") {
    assert(NPMSemver.gt(clue("1.0.0-109007199254740991"), clue("1.0.0-99007199254740991")))
    assert(NPMSemver.lt(clue("1.0.0-99007199254740991"), clue("1.0.0-109007199254740991")))
  }

  def assertMatchEquiv(version: String, matcher: String) =
    val Right(v) = parseVersion(version)
    val Right(m) = parseMatcher(matcher)

    //val matchingOptions = List(PreReleaseBehaviour.Loose -> MatchOptions.lenient, PreReleaseBehaviour.Strict -> MatchOptions.strict)

    val matches4s  = m.matches(v)
    val matchesNPM = NPMSemver.satisfies(version, matcher)
    val explain =
      if (matches4s) "semver4s matches, but npm doesn't"
      else "npm matches, but semver4s doesn't"
    assertEquals(matches4s, matchesNPM, clue((version, matcher, explain)))

  property("Semver4s and NPM are consistent in what versions satisfy a primitive range") {
    implicit val stringShrinker = shrinkMatcherString
    forAll(jsSafePrimitive, jsSafeVersion)((rs, v) => {
      assertMatchEquiv(v.format, rs)
    })
  }

  property("Semver4s and NPM are consistent in what versions satisfy a tilde range") {
    implicit val stringShrinker = shrinkMatcherString
    forAll(jsSafeTilde, jsSafeVersion)((rs, v) => {
      assertMatchEquiv(v.format, rs)
    })
  }

  test("Weird NPM behaviour that I don't think is to spec") {
    var matcherString = "~9007199254740991.9007199254740991.9007199254740991-1";
    var v =
      Version.unsafe(9007199254740991L, 9007199254740991L, 9007199254740991L, List(Left("-")), None)
    assert(!NPMSemver.satisfies(v.format, matcherString))
    var matcherString1 = "~1.1.1-1";
    var v1             = Version.unsafe(1, 1, 1, List(Left("-")), None)
    assert(NPMSemver.satisfies(v1.format, matcherString1))
  }

  property("Semver4s and NPM are consistent in gt and lt comparison") {
    import cats.syntax.all._
    forAll(jsSafeVersion, jsSafeVersion)((v1, v2) => {
      val gt4s  = v1 > v2
      val lt4s  = v1 < v2
      val gtnpm = NPMSemver.gt(v1.format, v2.format)
      val ltnpm = NPMSemver.lt(v1.format, v2.format)
      assertEquals(gtnpm, gt4s, clue(s"${v1.format} > ${v2.format}? npm says $gtnpm, we say $gt4s"))
      assertEquals(ltnpm, lt4s, clue(s"${v1.format} < ${v2.format}? npm says $ltnpm, we say $lt4s"))
    })
  }

/* this test frequently exhausts retryUntil
  property("Semver4s and NPM are consistent in what versions satisfy a range") {
    implicit val stringShrinker = shrinkMatcherString
    forAll(jsSafeMatcher, jsSafeVersion)((rs, v) => {
      val m = parseMatcher(rs).toOption.get
      //semver4s allows a bit more. Particularly, semver4s allows any numeric version part up to long
      //npm requires <= MaxSafeInteger
      val semver4sMatches = m.matches(v)
      val npmMatches      = NPMSemver.satisfies(v.format, rs)
      val explain =
        if semver4sMatches then "semver4s matches, but npm doesn't"
        else if npmMatches then "npm matches, but semver4s doesn't"
        else "neither matches"
      assertEquals(npmMatches, semver4sMatches, clue((rs, v.format, explain)))
    })
  }
 */
