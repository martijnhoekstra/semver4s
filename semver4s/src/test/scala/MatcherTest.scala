package semver4s

//import org.scalacheck.Prop.forAll

class MatcherTest extends munit.ScalaCheckSuite {
  def m(src: String) = matcher(src).toOption.get
  def v(src: String) = version(src).toOption.get

  test("comparator examples 1") {
    val range       = m(">=1.2.7")
    val matching    = List("1.2.7", "1.2.8", "2.5.3", "1.3.9").map(v)
    val notMatching = List("1.2.6", "1.1.0").map(v)
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("comparator set examples 2") {
    val range       = m(">=1.2.7 <1.3.0")
    val matching    = List("1.2.7", "1.2.8", "1.2.99").map(v)
    val notMatching = List("1.2.6", "1.3.0", "1.1.0").map(v)
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("comparator set examples 3") {
    val range       = m("1.2.7 || >=1.2.9 <2.0.0")
    val matching    = List("1.2.7", "1.2.9", "1.4.6").map(v)
    val notMatching = List("1.2.8", "2.0.0").map(v)
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("partial comparator >") {
    val range       = m(">2.12")
    val matching    = List("2.13.2", "2.13.0", "3.2.1").map(v)
    val notMatching = List("2.12.2", "2.12.0").map(v)
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("partial comparator >=") {
    val range       = m(">=2.12")
    val matching    = List("2.12.0", "2.12.10", "2.13.0", "3.0.0").map(v)
    val notMatching = List("2.11.0", "2.12.0-PRE1").map(v)
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("partial comparator =") {
    val range       = m("=2.12")
    val matching    = List("2.12.0", "2.12.10").map(v)
    val notMatching = List("2.11.0", "2.12.0-PRE1", "2.13.0", "3.0.0").map(v)
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("partial comparator <") {
    val range       = m("<2.13")
    val matching    = List("2.12.2", "2.12.0").map(v)
    val notMatching = List("2.13.2", "2.13.0", "3.2.1", "2.13.0-PRE1").map(v)
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("partial comparator <=") {
    val range       = m("<=2.13")
    val matching    = List("2.13.2", "2.13.0", "2.12.0", "2.12.1").map(v)
    val notMatching = List("2.14.2", "2.14.0", "3.2.1", "2.13.0-PRE1").map(v)
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("comparator prerelease") {
    val range       = m(">1.2.3-alpha.3")
    val matching    = List("1.2.3-alpha.7", "1.2.4").map(v)
    val notMatching = List("3.4.5-alpha.9").map(v)
    for (v <- matching) assert(clue(range).matches(clue(v)))
    for (v <- notMatching) assert(!clue(range).matches(clue(v)))
  }

  test("spec example") {
    val range   = "1.x || >=2.5.0 || 5.0.0 - 7.2.3"
    val matcher = RangeParsers.rangeSet.parseAll(range).toOption.get
    val version = SemverParsers.semver.parseAll("1.2.3").toOption.get
    assert(matcher.matches(version))
  }
}
