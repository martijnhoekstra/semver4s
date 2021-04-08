package semver4s

import org.scalacheck.Prop.forAll
import gen.GenMatcher._
import semver4s.parsing.MatcherParser

class MatcherParserTest extends munit.ScalaCheckSuite {

  test("primitive range examples") {
    val comparators = List(">", "=", "<", ">=", "<=").map(_ + "1.2.3")
    comparators.map(MatcherParser.primitive.parseAll).foreach { r =>
      assert(r.isRight, clue(r))
    }
  }

  property("all primitive ranges parse") {
    forAll(genPrimitive) { (range) =>
      {
        def parsed = MatcherParser.primitive.parseAll(range)
        assert(parsed.isRight, clue(range))
      }
    }
  }

  test("X range examples") {
    for {
      x      <- List("X", "x", "*")
      prefix <- List("1.", "1.2.")
    } {
      val result = MatcherParser.partial.parseAll(prefix + x)
      assert(result.isRight, clue(result))
    }
  }

  test("partial range examples") {
    List("1", "1.2").foreach { r =>
      val result = MatcherParser.partial.parseAll(r)
      assert(clue(result).isRight, clue(r))
    }
  }

  property("all partial and x ranges parse") {
    forAll(genPartial) { range =>
      def parsed = MatcherParser.partial.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
  }

  test("hyphen range examples") {
    List("1.2.3 - 2.3.4", "1.2 - 2.3.4", "1.2.3 - 2.3", "1.2.3 - 2").foreach { r =>
      val result = MatcherParser.hyphen.parseAll(r)
      assert(clue(result).isRight, clue(r))
    }
  }

  property("all hyphen ranges parse") {
    forAll(genHyphenRange) { range =>
      def parsed = MatcherParser.hyphen.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
  }

  test("tilde examples") {
    List("~1.2.3", "~1.2", "~1", "~0.2.3", "~0.2", "~0", "~1.2.3-beta.2").foreach { r =>
      val result = MatcherParser.tilde.parseAll(r)
      assert(clue(result).isRight, clue(r))
    }
  }

  property("all tilde ranges parse") {
    forAll(genTildeRange) { range =>
      def parsed = MatcherParser.tilde.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
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
    ).foreach { r =>
      val result = MatcherParser.caret.parseAll(r)
      assert(clue(result).isRight, clue(r))
    }
  }

  property("all caret ranges parse") {
    forAll(genCaretRange) { range =>
      def parsed = MatcherParser.caret.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
  }

  test("range examples") {
    List(">=1.2.7 <1.3.0").foreach { r =>
      val result = MatcherParser.range.parseAll(r)
      assert(clue(result).isRight, clue(r))
    }
  }

  property("all ranges parse") {
    forAll(genRange) { range =>
      def parsed = MatcherParser.range.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
  }

  test("range set examples") {
    List("1.x || >=2.5.0 || 5.0.0 - 7.2.3", "1.2.7 || >=1.2.9 <2.0.0").foreach { r =>
      val result = MatcherParser.rangeSet.parseAll(r)
      assert(clue(result).isRight, clue(r))
    }
  }

  property("all range sets parse") {
    forAll(genRangeSet) { range =>
      val parsed = MatcherParser.rangeSet.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
  }

  property("all range sets round-trip") {
    forAll(genRangeSet) { range =>
      for (matcher <- MatcherParser.rangeSet.parseAll(range)) {
        val formatted1 = Matcher.print(matcher)
        val matcher2   = MatcherParser.rangeSet.parseAll(formatted1).toOption.get
        val formatted2 = Matcher.print(matcher2)
        assertEquals(clue(formatted1), clue(formatted2))
        assertEquals(clue(matcher), clue(matcher2))
      }
    }
  }

}
