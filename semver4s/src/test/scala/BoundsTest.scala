package semver4s

import org.scalacheck.Prop.forAll
import cats.syntax.all._
import semver4s.gen.GenMatcher
import semver4s.gen.GenVersion
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
              clue(("version matches below inclusive lower bound", matcher, version))
            )
          case Exclusive(b) if version <= b =>
            assert(
              !matcher.matches(version),
              clue(("version matches below or at exclusive lower bound", matcher, version))
            )
          case _ => ()
        }
        upper match {
          case Inclusive(b) if version > b =>
            assert(
              !matcher.matches(version),
              clue(("version matches above inclusive upper bound", matcher, version))
            )
          case Exclusive(b) if version >= b =>
            assert(
              !matcher.matches(version),
              clue(("version matches above or at exclusive upper bound", matcher, version))
            )
          case _ => ()
        }
      }
    }
  }
}
