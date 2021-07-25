package semver4s.coreparse

import org.scalacheck.Prop.forAll
import semver4s._
import gen.GenMatcher._

//most tests in terms of same result as the cats parsers, which are tested
//more extensively in its own test suite
class SemverCoreMatcherTest extends munit.ScalaCheckSuite {
  import coreparse.MatcherParser

  def coreEqCats(matcherString: String) = for {
    core <- MatcherParser.parseMatcher(matcherString)
    cats <- semver4s.parsing.MatcherParser.matcher.parseAll(matcherString)
  } assertEquals(cats, core, matcherString)

  test("primitive range examples") {
    val comparators = List(">", "=", "<", ">=", "<=").map(_ + "1.2.3")
    comparators.foreach(coreEqCats)
  }

  property("all primitive ranges parse") {
    forAll(genPrimitive)(coreEqCats)
  }

  test("partial range VersionLike") {
    val Right(vl) = SharedParser.parseVersionLike("1.2")
    assertEquals(vl.numbers, List(1L, 2L))
    val partial = MatcherParser.versionLikeToPartial(vl)
    partial match {
      case Right(Partial.Minor(1L, 2L)) => ()
      case _                            => assert(false, partial)
    }
  }

  test("partial range example minor") {
    MatcherParser.parseMatcher("1.2") match {
      case Right(Matcher.Or(List(Matcher.And(List(Matcher.Exact(xs)))))) =>
        assertEquals(xs, Partial.unsafe(1L, 2L))
      case x => assert(false, x)
    }
  }

  test("partial range examples") {
    coreEqCats("1")
    coreEqCats("1.2")
  }

  test("X range examples") {
    for {
      x      <- List("X", "x", "*")
      prefix <- List("1.", "1.2.")
    } coreEqCats(prefix + x)
  }

  property("all partial and x ranges parse") {
    forAll(genPartial)(coreEqCats)
  }

  test("hyphen range examples") {
    List("1.2.3 - 2.3.4", "1.2 - 2.3.4", "1.2.3 - 2.3", "1.2.3 - 2").foreach(coreEqCats)
  }

  property("all hyphen ranges parse") {
    forAll(genHyphenRange)(coreEqCats)
  }

  test("tilde examples") {
    List("~1.2.3", "~1.2", "~1", "~0.2.3", "~0.2", "~0", "~1.2.3-beta.2").foreach(coreEqCats)
  }

  property("all tilde ranges parse") {
    forAll(genTildeRange)(coreEqCats)
  }

  test("caret examples") {
    List(
      "^1.2.3",
      "^0.2.3",
      "^0.0.3",
      "^1.2.3-beta.2",
      "^0.0.3-beta",
      "^1.2.x",
      "^0.0.x",
      "^0.0",
      "^0.x",
      "^1.x"
    ).foreach(coreEqCats)
  }

  property("all caret ranges parse") {
    forAll(genCaretRange)(coreEqCats)
  }

  test("range examples") {
    List(">=1.2.7 <1.3.0").foreach(coreEqCats)
  }

  property("all ranges parse") {
    forAll(genRange)(coreEqCats)
  }

  test("range set examples") {
    List("1.x || >=2.5.0 || 5.0.0 - 7.2.3", "1.2.7 || >=1.2.9 <2.0.0").foreach(coreEqCats)
  }

  property("all range sets parse") {
    forAll(genRangeSet)(coreEqCats)
  }

  property("all range sets round-trip") {
    forAll(genRangeSet) { range =>
      for (matcher <- MatcherParser.parseMatcher(range)) {
        val formatted1 = Matcher.print(matcher)
        val matcher2   = MatcherParser.parseMatcher(formatted1).toOption.get
        val formatted2 = Matcher.print(matcher2)
        assertEquals(clue(formatted1), clue(formatted2))
        assertEquals(clue(matcher), clue(matcher2))
      }
    }
  }

}
