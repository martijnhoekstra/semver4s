package semver4s

import org.scalacheck.Shrink
import semver4s.Matcher._
import cats.data.NonEmptyList
import scala.annotation.nowarn
import semver4s.Partial._
import semver4s.SemVer._

@nowarn("cat=deprecation")
object Shrinkers {

  def shrinkSuffix(s: PreReleaseSuffix): Stream[PreReleaseSuffix] = {
    implicitly[Shrink[List[Either[String, Long]]]]
      .shrink(s.toList)
      .map(_.filterNot(_.fold(_ == "", _ <= 0)))
      .collect { case head :: tail =>
        NonEmptyList(head, tail)
      }
  }

  implicit val sSuffix: Shrink[PreReleaseSuffix] = Shrink(shrinkSuffix(_))

  implicit val shrinkVersion: Shrink[Version] = Shrink { case Version(maj, min, pat, pre, _) =>
    Version.unsafe(maj, min, pat, pre, None) #:: implicitly[Shrink[
      (Long, Long, Long, Option[PreReleaseSuffix])
    ]].shrink((maj, min, pat, pre)).flatMap {
      case (maj1, min1, pat1, Some(pre1)) => Version(maj1, min1, pat1, pre1)
      case (maj1, min1, pat1, None)       => Version(maj1, min1, pat1)
    }
  }

  implicit val shrinkPartial: Shrink[Partial] = {

    def shrinkPat(pat: Patch): Stream[Partial] = pat match {
      case Patch(maj, min, pat) =>
        implicitly[Shrink[(Long, Long, Long)]].shrink((maj, min, pat)).flatMap {
          case (0, 0, 0)          => shrinkMin(Partial.unsafe(maj, min))
          case (maj1, min1, pat1) => Partial(maj1, min1, pat1).toStream
        }

    }
    def shrinkMaj(maj: Major): Stream[Partial] =
      Shrink
        .shrinkIntegral[Long]
        .shrink(maj.major)
        .takeWhile(_ > 0)
        .map(Partial.unsafe(_)) #::: Stream(Wild)
    def shrinkMin(m: Minor): Stream[Partial] = m match {
      case Minor(0, x) =>
        Shrink.shrinkIntegral[Long].shrink(x).takeWhile(_ > 0).map(Partial.unsafe(0, _))
      case Minor(x, y) =>
        implicitly[Shrink[(Long, Long)]].shrink((x, y)).flatMap {
          case (0, 0)   => shrinkMaj(Partial.unsafe(x))
          case (x1, y1) => Partial(x1, y1).toStream
        }
    }

    Shrink {
      case Pre(major, minor, patch, pre) => {
        val pat = Partial.unsafe(major, minor, patch)
        shrinkSuffix(pre).map(p => Partial.unsafe(major, minor, patch, p)) #::: pat #:: shrinkPat(
          pat
        )
      }
      case patch: Patch => shrinkPat(patch)
      case min: Minor   => shrinkMin(min)
      case maj: Major   => shrinkMaj(maj)
      case Wild         => Stream.empty
    }
  }

  implicit def shrinkMatcher: Shrink[Matcher] = {
    def shrinkAnd(and: Matcher.And): Stream[Matcher.And] =
      implicitly[Shrink[List[Matcher.Simple]]].shrink(and.simples.toList).flatMap {
        case Nil          => Stream.empty
        case head :: tail => Stream(Matcher.And(NonEmptyList(head, tail)))
      }
    implicit val andShrinker = Shrink(shrinkAnd)
    def shrinkOr(or: Matcher.Or): Stream[Matcher.Or] =
      implicitly[Shrink[List[Matcher.And]]].shrink(or.ands.toList).flatMap {
        case Nil          => Stream.empty
        case head :: tail => Stream(Matcher.Or(NonEmptyList(head, tail)))
      }

    Shrink {
      case or: Or   => shrinkOr(or)
      case and: And => shrinkAnd(and)
      case LT(p)    => shrinkPartial.shrink(p).map(LT(_))
      case LTE(p)   => shrinkPartial.shrink(p).map(LTE(_))
      case GT(p)    => shrinkPartial.shrink(p).map(GT(_))
      case GTE(p)   => shrinkPartial.shrink(p).map(GTE(_))
      case Caret(p) => shrinkPartial.shrink(p).map(Caret(_))
      case Exact(p) => shrinkPartial.shrink(p).map(Exact(_))
      case Tilde(p) => shrinkPartial.shrink(p).map(Tilde(_))
      case Hyphen(lower, upper) =>
        Shrink.shrinkTuple2[Partial, Partial].shrink(lower -> upper).map { case (l, u) =>
          Hyphen(l, u)
        }
    }
  }

  def shrinkMatcherString: Shrink[String] = Shrink[String](s =>
    semver4s
      .parseMatcher(s)
      .fold(_ => Stream.empty, m => shrinkMatcher.shrink(m).map(_.format))
  )

  def shrinkVersionString: Shrink[String] = Shrink[String](s =>
    semver4s
      .parseVersion(s)
      .fold(_ => Stream.empty, v => shrinkVersion.shrink(v).map(_.format))
  )
}
