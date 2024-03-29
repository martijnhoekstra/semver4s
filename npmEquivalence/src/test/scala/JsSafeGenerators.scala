package semver4s.gen

import cats.data.NonEmptyList
import org.scalacheck.Gen
import semver4s.gen.GenMatcher._
import semver4s.gen.GenVersion._
import semver4s._

object JsSafeGenerators:

  val maxSafeInt = 9007199254740991L - 1

  def isJsSafeVersion(v: Version) =
    v.major <= maxSafeInt && v.minor <= maxSafeInt && v.patch <= maxSafeInt && v.format.length <= 256

  def isJsSafeMatcher(m: Matcher): Boolean = m match
    case Matcher.And(simples)     => simples.forall(isJsSafeMatcher)
    case Matcher.Or(ands)         => ands.forall(isJsSafeMatcher)
    case Matcher.Hyphen(from, to) => isJsSafeVersion(from.version) && isJsSafeVersion(to.version)
    case Matcher.Caret(base)      => isJsSafeVersion(base.version)
    case Matcher.Tilde(base)      => isJsSafeVersion(base.version)
    case s: Matcher.Primitive     => isJsSafeVersion(s.p.version)

  def cutSuffix(p: Partial) = p match
    case Partial.Pre(major, minor, patch, pre) =>
      Partial.unsafe(
        math.min(major, maxSafeInt),
        math.min(minor, maxSafeInt),
        math.min(patch, maxSafeInt),
        pre.take(3).map {
          case Right(i)  => Right(i % 1000)
          case Left(str) => Left(str.take(5))
        }
      )
    case _ => p

  def cutPreSuffixes(m: Matcher): Matcher = m match
    case Matcher.And(simples) =>
      Matcher.And(simples.take(3).map(cutPreSuffixes).asInstanceOf[List[Matcher.Simple]])
    case Matcher.Or(ands) =>
      Matcher.Or(ands.take(3).map(cutPreSuffixes).asInstanceOf[List[Matcher.And]])
    case Matcher.Hyphen(from, to) => Matcher.Hyphen(cutSuffix(from), cutSuffix(to))
    case Matcher.Caret(base)      => Matcher.Caret(cutSuffix(base))
    case Matcher.Tilde(base)      => Matcher.Tilde(cutSuffix(base))
    case Matcher.LTE(base)        => Matcher.LTE(cutSuffix(base))
    case Matcher.LT(base)         => Matcher.LT(cutSuffix(base))
    case Matcher.GT(base)         => Matcher.GT(cutSuffix(base))
    case Matcher.GTE(base)        => Matcher.GTE(cutSuffix(base))
    case Matcher.Exact(base)      => Matcher.Exact(cutSuffix(base))

  def cutToJsSafeLength(matcherString: String): Option[String] =
    val max = 256
    if (matcherString.length <= max) Some(matcherString)
    else
      val first = matcherString.indexOf(" || ")
      if (first == -1 || first > max) None
      else
        def loop(prev: Int): String =
          // "012 || 789 || 456" ->
          //  prev = -1, next = 3
          //  prev = 3, next = 11
          //  prev = 11, next = -1
          //  substr(0, 11)
          val next = matcherString.indexOf(" || ", prev + 1)
          if (next == -1 || next > max) matcherString.substring(0, prev)
          else
            loop(next)
        Some(loop(first))

  def shorten(v: Version): Version = v match
    case Version(major, minor, patch, Nil, _) =>
      Version.unsafe(
        math.min(major, maxSafeInt),
        math.min(minor, maxSafeInt),
        math.min(patch, maxSafeInt)
      )
    case Version(major, minor, patch, pre, _) =>
      Version.unsafe(
        math.min(major, maxSafeInt),
        math.min(minor, maxSafeInt),
        math.min(patch, maxSafeInt),
        pre.take(3).map {
          case Right(i)  => Right(i % 1000)
          case Left(str) => Left(str.take(5))
        }
      )

  def jsSafe(genMatcher: Gen[String], maxTries: Int) = genMatcher
    .map(m => cutPreSuffixes(parseMatcher(m).toOption.get).format)
    .flatMap(x => {
      cutToJsSafeLength(x) match {
        case Some(m) => Gen.const(m)
        case None    => Gen.fail
      }
    })
    .retryUntil(s => s.length <= 256 && parseMatcher(s).exists(isJsSafeMatcher), maxTries)

  val jsSafeVersion = genVersion
    .map {
      case long if !isJsSafeVersion(long) => shorten(long)
      case safe                           => safe
    }
    .retryUntil(isJsSafeVersion, 20)
  val jsSafePrimitive = jsSafe(genPrimitive, 20)
  val jsSafeTilde     = jsSafe(genTildeRange, 20)
  val jsSafeMatcher   = jsSafe(genRangeSet, 20)
