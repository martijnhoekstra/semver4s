package semver4s

import org.scalacheck.Prop.forAll
import cats.syntax.all._
//import Shrinkers._
import Bound._

class MatcherBoundsTest extends munit.ScalaCheckSuite {

  property("versions not within bounds never match") {
    forAll(GenVersion.genVersion, GenMatcher.genMatcher) {
      case (version, matcher) => {
        val lower = Matcher.lowerBound(matcher)
        val upper = Matcher.upperBound(matcher)
        lower match {
          case Inclusive(b) if version < b =>
            assert(
              !matcher.matches(version),
              clue((matcher, version, "version matches below inclusive lower bound"))
            )
          case Exclusive(b) if version <= b =>
            assert(
              !matcher.matches(version),
              clue((matcher, version, "version matches below or at exclusive lower bound"))
            )
          case _ => ()
        }
        upper match {
          case Inclusive(b) if version > b =>
            assert(
              !matcher.matches(version),
              clue((matcher, version, "version matches above inclusive upper bound"))
            )
          case Exclusive(b) if version >= b =>
            assert(
              !matcher.matches(version),
              clue((matcher, version, "version matches above or at exclusive upper bound"))
            )
          case _ => ()
        }
      }
    }
  }
}
