package semver4s

import org.scalacheck.Prop.forAll
import org.scalacheck.Shrink

class NpmEquivalenceTest extends munit.ScalaCheckSuite {
  import semver4s.npm.NPMSemver
  import semver4s.GenMatcher._
  import semver4s.GenVersion._

  val maxSafeInt = 9007199254740991L

  def isJsSafeVersion(v: Version) =
    v.major <= maxSafeInt && v.minor <= maxSafeInt && v.patch <= maxSafeInt && v.format.length <= 256

  def isJsSafeMatcher(m: Matcher): Boolean = m match {
    case Matcher.And(simples)     => simples.forall(isJsSafeMatcher)
    case Matcher.Or(ands)         => ands.forall(isJsSafeMatcher)
    case Matcher.Hyphen(from, to) => isJsSafeVersion(from.version) && isJsSafeVersion(to.version)
    case Matcher.Caret(base)      => isJsSafeVersion(base.version)
    case Matcher.Tilde(base)      => isJsSafeVersion(base.version)
    case s: Matcher.Primitive     => isJsSafeVersion(s.p.version)
  }

  val jsSafeVersion = genVersion.retryUntil(isJsSafeVersion)
  val jsSafePrimitive =
    genPrimitive.retryUntil(s => s.length <= 256 && parseMatcher(s).exists(isJsSafeMatcher))
  val jsSafeTilde =
    genTildeRange.retryUntil(s => s.length <= 256 && parseMatcher(s).exists(isJsSafeMatcher))
  val jsSafeMatcher =
    genRangeSet.retryUntil(s => s.length <= 256 && parseMatcher(s).exists(isJsSafeMatcher))

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

  def assertMatchEquiv(version: String, matcher: String) = {
    val Right(v) = parseVersion(version)
    val Right(m) = parseMatcher(matcher)

    //val matchingOptions = List(PreReleaseBehaviour.Loose -> MatchOptions.lenient, PreReleaseBehaviour.Strict -> MatchOptions.strict)

    val matches4s  = m.matches(v)
    val matchesNPM = NPMSemver.satisfies(version, matcher)
    val explain =
      if (matches4s) "semver4s matches, but npm doesn't"
      else "npm matches, but semver4s doesn't"
    assertEquals(matches4s, matchesNPM, clue((version, matcher, explain)))
  }

  property("Semver4s and NPM are consistent in what versions satisfy a primitive range") {
    implicit def noShrink[A]: Shrink[A] = Shrink.shrinkAny
    /*
    debug stuff for timings (hint: genMatcher is really slow. But why? todo: AsyncProfiler that shit)
    import java.time._
    var last = Instant.now()
    val durations = new scala.collection.mutable.ListBuffer[(String, Duration, Duration)]()
    def showTimings() = {
      println("The timings!")
      println("")
      val ds = durations.toList
      println("top by gen time:")
      ds.sortBy{ case (_, _, d) => (d.getSeconds(), d.getNano)}.reverse.take(3).zipWithIndex.foreach {
        case ((str, _, d), i) => println(s"(${i + 1}) $str: $d")
      }
      println("top by test time:")
      ds.sortBy{ case (_, d, _) => (d.getSeconds(), d.getNano)}.reverse.take(3).zipWithIndex.foreach {
        case ((str, d, _), i) => println(s"(${i + 1}) $str: $d")
      }
    }
    var i = 0; */
    forAll(jsSafePrimitive, jsSafeVersion)((rs, v) => {
      //val startTest = Instant.now()
      assertMatchEquiv(v.format, rs)
      //val endTest = Instant.now()
      //val testDuration = Duration.between(startTest, endTest)
      //val genDuration = Duration.between(last, startTest)
      //durations += ((rs, testDuration, genDuration))
      //last = endTest
      //i += 1
      //if (i == 100) showTimings()
    })
  }

  property("Semver4s and NPM are consistent in what versions satisfy a tilde range") {
    implicit def noShrink[A]: Shrink[A] = Shrink.shrinkAny
    forAll(jsSafeTilde, jsSafeVersion)((rs, v) => {
      assertMatchEquiv(v.format, rs)
    })
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

  /* this test is too damn slow
     I think it's the generators
  property("Semver4s and NPM are consistent in what versions satisfy a range") {
    implicit def noShrink[A]: Shrink[A] = Shrink.shrinkAny
    forAll(jsSafeMatcher, jsSafeVersion)((rs, v) => {
      val m = parseMatcher(rs).toOption.get
      //semver4s allows a bit more. Particularly, semver4s allows any numeric version part up to long
      //npm requires <= MaxSafeInteger
      val versionString = v.format
      val semver4sMatches = m.matches(v)
      val npmMatches = NPMSemver.satisfies(clue(versionString), clue(rs))
      val explain = if (semver4sMatches) "semver4s matches, but npm doesn't"
                    else "npm matches, but semver4s doesn't"
      assertEquals(semver4sMatches, npmMatches, clue((rs, v.format, explain)))
    })
  }
   */

}
