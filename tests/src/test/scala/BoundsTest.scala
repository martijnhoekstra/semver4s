package semver4s

import org.scalacheck.Prop.forAll
import math.Ordering.Implicits.infixOrderingOps
import cats.syntax.all._
import scala.util.Try
import scala.util.Success
import semver4s.gen.GenMatcher
import semver4s.gen.GenVersion
//import Shrinkers._
import Bound._

class MatcherBoundsTest extends munit.ScalaCheckSuite {

  property("versions not within bounds never match") {
    forAll(GenVersion.genVersion, GenMatcher.genMatcher) {
      case (version, matcher) => {
        val lower   = Matcher.lowerBound(matcher)
        val upper   = Try(Matcher.upperBound(matcher))
        val matches = matcher.matches(version)
        lower match {
          case Inclusive(b) if version < b =>
            assert(
              !matches,
              clue(("version matches below inclusive lower bound", matcher, version))
            )
          case Exclusive(b) if version <= b =>
            assert(
              !matches,
              clue(("version matches below or at exclusive lower bound", matcher, version))
            )
          case _ => ()
        }
        upper match {
          case Success(Inclusive(b)) if version > b =>
            assert(
              !matches,
              clue(("version matches above inclusive upper bound", matcher, version))
            )
          case Success(Exclusive(b)) if version >= b =>
            assert(
              !matches,
              clue(("version matches above or at exclusive upper bound", matcher, version))
            )
          case _ => ()
        }
      }
    }
  }

  property("versions that match are within bounds") {
    forAll(GenVersion.genVersion, GenMatcher.genMatcher) {
      case (version, matcher) => {
        val lower         = Matcher.lowerBound(matcher)
        val upper         = Try(Matcher.upperBound(matcher))
        val matchesLoose  = matcher.matches(version, PreReleaseBehaviour.Loose)
        val matchesStrict = matcher.matches(version, PreReleaseBehaviour.Strict)
        if (matchesLoose || matchesStrict) {
          lower match {
            case Exclusive(by) => assert(version > by)
            case Inclusive(by) => assert(version >= by)
            case Unbounded     => ()
          }
          upper match {
            case Success(Exclusive(by)) => assert(!(version >= by))
            case Success(Inclusive(by)) => assert(!(version > by))
            case _                      => ()
          }
        }
      }
    }
  }
}
