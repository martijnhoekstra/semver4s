package semver4s

import PreReleaseBehaviour._
import org.scalacheck.Prop.forAll
import Literal._

class MatcherTest extends munit.ScalaCheckSuite {

  def areEquivalent(m1: Matcher, m2: Matcher) = forAll(GenVersion.genVersion) { v =>
    //test both "default" and all explicit pre-release behaviours

    assertEquals(
      m1.matches(v),
      m2.matches(v),
      clue((Matcher.print(m1), Matcher.print(m2), v.format))
    )

    for (
      behaviour <- List(
        PreReleaseBehaviour.Loose,
        PreReleaseBehaviour.Strict,
        PreReleaseBehaviour.Never
      )
    ) {
      assertEquals(
        m1.matches(v, behaviour),
        m2.matches(v, behaviour),
        clue((Matcher.print(m1), Matcher.print(m2), v.format, behaviour))
      )
    }
  }

  test("comparator examples 1") {
    val range       = m">=1.2.7"
    val matching    = List(v"1.2.7", v"1.2.8", v"2.5.3", v"1.3.9")
    val notMatching = List(v"1.2.6", v"1.1.0")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("comparator set examples 2") {
    val range       = m">=1.2.7 <1.3.0"
    val matching    = List(v"1.2.7", v"1.2.8", v"1.2.99")
    val notMatching = List(v"1.2.6", v"1.3.0", v"1.1.0")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("comparator set examples 3") {
    val range       = m"1.2.7 || >=1.2.9 <2.0.0"
    val matching    = List(v"1.2.7", v"1.2.9", v"1.4.6")
    val notMatching = List(v"1.2.8", v"2.0.0")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("partial comparator >") {
    val range       = m">2.12"
    val matching    = List(v"2.13.2", v"2.13.0", v"3.2.1")
    val notMatching = List(v"2.12.2", v"2.12.0")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("partial comparator >=") {
    val range       = m">=2.12"
    val matching    = List(v"2.12.0", v"2.12.10", v"2.13.0", v"3.0.0")
    val notMatching = List(v"2.11.0", v"2.12.0-PRE1")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("partial comparator =") {
    val range       = m"=2.12"
    val matching    = List(v"2.12.0", v"2.12.10")
    val notMatching = List(v"2.11.0", v"2.12.0-PRE1", v"2.13.0", v"3.0.0")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("partial comparator <") {
    val range       = m"<2.13"
    val matching    = List(v"2.12.2", v"2.12.0")
    val notMatching = List(v"2.13.2", v"2.13.0", v"3.2.1", v"2.13.0-PRE1")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("partial comparator < with pre") {
    val range       = m"<2.13"
    val matching    = List(v"2.12.2", v"2.12.0", v"2.12.3-PRE1")
    val notMatching = List(v"2.13.2", v"2.13.0", v"3.2.1", v"2.13.0-PRE1")
    for (v <- matching) assert(clue(range).matches(clue(v), clue(Loose)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v), clue(Loose)))
  }

  test("partial comparator <=") {
    val range       = m"<=2.13"
    val matching    = List(v"2.13.2", v"2.13.0", v"2.12.0", v"2.12.1")
    val notMatching = List(v"2.14.2", v"2.14.0", v"3.2.1", v"2.13.0-PRE1")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("comparator > prerelease") {
    val range       = m">1.2.3-alpha.3"
    val matching    = List(v"1.2.3-alpha.7", v"1.2.3-rc1", v"1.2.3", v"1.2.4")
    val notMatching = List(v"3.4.5-alpha.9", v"1.2.3-RC1")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("comparator > prerelease loose") {
    val range = m">1.2.3-alpha.3"
    val matching = List(
      v"1.2.3-alpha.7",
      v"1.2.3-rc1",
      v"1.2.3",
      v"3.4.5",
      v"3.4.5-alpha.9",
      v"1.2.4",
      v"1.2.3-alpha.3alpha"
    )
    val notMatching = List(v"1.2.3-alpha.2", v"1.2.3-RC1")
    for (v <- matching) assert(clue(range).matches(clue(v), clue(Loose)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v), clue(Loose)))
  }

  test("caret full") {
    val range       = m"^2.12.13"
    val matching    = List(v"2.12.13", v"2.12.20", v"2.13.0", v"2.13.1", v"2.14.0")
    val notMatching = List(v"3.0.0", v"3.0.0-RC1", v"3.1.0")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("caret wild patch") {
    val range       = m"^1.2.*"
    val matching    = List(v"1.2.0", v"1.2.1", v"1.3.0", v"1.14323701.0")
    val notMatching = List(v"2.0.0", v"2.0.1", v"1.1.1")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  property("caret equivalents from npm documentation") {
    val equivalences = List(
      m"^1.2.3"      -> m">=1.2.3 <2.0.0",
      m"^0.2.3"      -> m">=0.2.3 <0.3.0",
      m"^0.0.3"      -> m">=0.0.3 <0.0.4",
      m"^0.0.3-beta" -> m">=0.0.3-beta <0.0.4",
      m"^1.2.x"      -> m">=1.2.0 <2.0.0",
      m"^0.0.x"      -> m">=0.0.0 <0.1.0",
      m"^0.0"        -> m">=0.0.0 <0.1.0",
      m"^1.x"        -> m">=1.0.0 <2.0.0",
      m"^0.x"        -> m">=0.0.0 <1.0.0"
    )
    equivalences
      .map { case (m1, m2) => areEquivalent(m1, m2) }
      .reduce(_ && _)
  }

  property("tilde equivalents from npm documentation") {
    val equivalences = List(
      m"~1.2.3"        -> m">=1.2.3 <1.3.0",
      m"~1.2"          -> m">=1.2.0 <1.3.0",
      m"~1.2"          -> m"1.2.x",
      m"~1"            -> m">=1.0.0 <2.0.0",
      m"~1"            -> m"1.x",
      m"~0.2.3"        -> m">=0.2.3 <0.3.0",
      m"~0.2"          -> m">=0.2.0 <0.3.0",
      m"~0.2"          -> m"0.2.x",
      m"~0"            -> m">=0.0.0 <1.0.0",
      m"~0"            -> m"0.x",
      m"~1.2.3-beta.2" -> m">=1.2.3-beta.2 <1.3.0"
    )

    equivalences
      .map { case (m1, m2) => areEquivalent(m1, m2) }
      .reduce(_ && _)
  }

  property("caret equivalent to range") {
    val tilde = m"^2.12.13"
    val range = m"2.12.13 - 3"
    areEquivalent(tilde, range)
  }

  test("hyphen range examples right partial") {
    val range       = m"2.12.13 - 3"
    val matching    = List(v"2.12.13", v"2.12.20", v"2.13.0", v"2.13.1", v"2.14.0")
    val notMatching = List(v"3.0.0", v"3.0.0-RC1", v"3.1.0")
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("spec example") {
    val matcher = m"1.x || >=2.5.0 || 5.0.0 - 7.2.3"
    val version = v"1.2.3"
    assert(matcher.matches(version))
  }
}
