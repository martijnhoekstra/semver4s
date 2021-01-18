package semver4s

import org.scalacheck.Prop.forAll

class RangeParserTest extends munit.ScalaCheckSuite {
  import GenMatcher._

  test("primitive range examples") {
    val comparators = List(">", "=", "<", ">=", "<=").map(_ + "1.2.3")
    comparators.map(RangeParsers.primitive.parseAll).foreach { r =>
      assert(r.isRight, clue(r))
    }
  }

  property("all primitive ranges parse") {
    forAll(genPrimitive) { (range) =>
      {
        def parsed = RangeParsers.primitive.parseAll(range)
        assert(parsed.isRight, clue(range))
      }
    }
  }

  test("X range examples") {
    for {
      x      <- List("X", "x", "*")
      prefix <- List("1.", "1.2.")
    } {
      val result = RangeParsers.partial.parseAll(prefix + x)
      assert(result.isRight, clue(result))
    }
  }

  test("partial range examples") {
    List("1", "1.2").foreach { r =>
      val result = RangeParsers.partial.parseAll(r)
      assert(result.isRight, clues(clue(r), clue(result)))
    }
  }

  property("all partial and x ranges parse") {
    forAll(genPartial) { range =>
      def parsed = RangeParsers.partial.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
  }

  test("hyphen range examples") {
    List("1.2.3 - 2.3.4", "1.2 - 2.3.4", "1.2.3 - 2.3", "1.2.3 - 2").foreach { r =>
      val result = RangeParsers.hyphen.parseAll(r)
      assert(result.isRight, clues(clue(r), clue(result)))
    }
  }

  property("all hyphen ranges parse") {
    forAll(genHyphenRange) { range =>
      def parsed = RangeParsers.hyphen.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
  }

  test("tilde examples") {
    List("~1.2.3", "~1.2", "~1", "~0.2.3", "~0.2", "~0", "~1.2.3-beta.2").foreach { r =>
      val result = RangeParsers.tilde.parseAll(r)
      assert(result.isRight, clues(clue(r), clue(result)))
    }
  }

  property("all tilde ranges parse") {
    forAll(genTildeRange) { range =>
      def parsed = RangeParsers.tilde.parseAll(range)
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
      val result = RangeParsers.caret.parseAll(r)
      assert(result.isRight, clues(clue(r), clue(result)))
    }
  }

  property("all caret ranges parse") {
    forAll(genCaretRange) { range =>
      def parsed = RangeParsers.caret.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
  }

  test("range examples") {
    List(">=1.2.7 <1.3.0").foreach { r =>
      val result = RangeParsers.range.parseAll(r)
      assert(result.isRight, clues(clue(r), clue(result)))
    }
  }

  property("all ranges parse") {
    forAll(genRange) { range =>
      def parsed = RangeParsers.range.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
  }

  test("range set examples") {
    List("1.x || >=2.5.0 || 5.0.0 - 7.2.3", "1.2.7 || >=1.2.9 <2.0.0").foreach { r =>
      val result = RangeParsers.rangeSet.parseAll(r)
      assert(result.isRight, clues(clue(r), clue(result)))
    }
  }

  property("all range sets parse") {
    forAll(genRangeSet) { range =>
      def parsed = RangeParsers.rangeSet.parseAll(range)
      assert(parsed.isRight, clue(range))
    }
  }

}
