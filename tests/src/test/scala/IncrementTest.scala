package semver4s

import org.scalacheck.Prop.forAll
import semver4s.gen.GenVersion
import semver4s.gen.GenMatcher
import semver4s.gen.GenVersion._
import scala.math.Ordering.Implicits._
import scala.annotation.nowarn

class IncrementTest extends munit.ScalaCheckSuite {

  final val MaxLong = Long.MaxValue

  property("incremented versions are larger than their original") {
    forAll(genVersion)((v: Version) => {
      assert(v.increment > v, "increment")
      if (v.major != MaxLong) {
        assert(v.incrementMajor > v, "increment major")
      }
      if (v.minor != MaxLong) {
        assert(v.incrementMinor > v, "increment minor")
      }
      if (v.patch != MaxLong) {
        assert(v.incrementPatch > v, "increment patch")
      }
    })
  }

  property("incremented partial versions are larger than their original") {
    import Partial._
    def overflows[T](body: => T) = intercept[ArithmeticException](body).void
    forAll(genPartial)({
      case m @ Major(MaxLong)                     => overflows(m.increment)
      case mn @ Minor(MaxLong, MaxLong)           => overflows(mn.increment)
      case mn @ Minor(MaxLong, _)                 => overflows(mn.incrementMajor)
      case pt @ Patch(MaxLong, MaxLong, MaxLong)  => overflows(pt.increment)
      case pt @ Patch(MaxLong, MaxLong, _)        => overflows(pt.incrementMinor)
      case pt @ Patch(MaxLong, _, _)              => overflows(pt.incrementMajor)
      case pr @ Pre(MaxLong, MaxLong, MaxLong, _) => overflows(pr.incrementPatch)
      case pr @ Pre(MaxLong, MaxLong, _, _)       => overflows(pr.incrementMinor)
      case pr @ Pre(MaxLong, _, _, _)             => overflows(pr.incrementMajor)
      case Wild                                   => assert(Wild.increment == Wild)
      case p                                      => assert(p.increment.version > p.version)
    })
  }

  property("incremented partial versions match the same > as their original") {
    forAll(GenVersion.genVersion, GenMatcher.genPartial)((v, p) => {
      val matcher = parsing.MatcherParser.rangeSet.parseAll(s">$p").toOption.get
      if (matcher.matches(v)) assert(matcher.matches(v.increment))
    })
  }

  implicit class Void[T](t: T) {
    @nowarn // returning unit isn't all that big a side-effect
    def void = ()
  }

}
