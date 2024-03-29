package semver4s

import math.Ordering.Implicits.infixOrderingOps
import semver4s.PreReleaseBehaviour._
import Bound._

/** A matcher that can test whether a version matches the matcher.
  *
  * The NPM matchers are
  *
  *   - Primitive matchers >, <, >=, <= or = with a (potentially partial) version, e.g. `<1.3` or
  *     `>=1.3.4-pre.1`.
  *   - Hyphen ranges, e.g. 1.3 - 2
  *   - Tilde ranges: e.g. ~1.3.2 flexible patch.
  *   - Caret ranges: e.g. ^1.3.2 flexible minor and patch.
  *   - Wildcard ranges: e.g. 1.3 / 1.3.* / 1.3.x
  *   - Conjunction (AND) ranges: e.g. >1.3 <2.3.1
  *   - Disjunction (OR) ranges: e.g. any version in three any version in 1.2: 1.2 3 || 1.2.*
  */
sealed trait Matcher extends Product with Serializable {
  override def toString = format
  def format: String    = Matcher.print(this)
  def matches(that: Version): Boolean
  def matches(that: Version, pre: PreReleaseBehaviour): Boolean
}

object Matcher {
  import Partial._

  // If a version has a prerelease tag (for example, 1.2.3-alpha.3) then it will only be allowed to satisfy comparator
  // sets if at least one comparator with the same [major, minor, patch] tuple also has a prerelease tag.

  /** Simple matchers: all matcher except conjunction and disjunction (AND and OR) ranges
    */
  sealed trait Simple extends Matcher

  /** Hyphen ranges, ranges bounded by a lower and upper bound
    */
  case class Hyphen(lower: Partial, upper: Partial) extends Simple {
    def matches(version: Version): Boolean = matches(version, Strict)
    def matches(version: Version, pre: PreReleaseBehaviour): Boolean =
      lower.version <= version && lte(upper).matches(version, pre)
  }

  /** Caret ranges, flexible minor and patch, special handling for 0 prefixes
    */
  case class Caret(base: Partial) extends Simple {
    def matches(that: Version): Boolean = matches(that, Strict)
    def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean = {
      val lower = gte(base)
      base match {
        case Wild => true // ^* kinda weird, but whatever
        case Major(Long.MaxValue) | Minor(Long.MaxValue, _) | Patch(Long.MaxValue, _, _) |
            Pre(Long.MaxValue, _, _, _) =>
          lower.matches(that, preBehaviour)
        case m @ Major(_)       => (lower && lt(m.increment)).matches(that, preBehaviour)
        case m @ Minor(0, _)    => (lower && lt(m.increment)).matches(that, preBehaviour)
        case m @ Minor(_, _)    => (lower && lt(m.incrementMajor)).matches(that, preBehaviour)
        case Patch(0, 0, _)     => Matcher.eqv(base).matches(that, preBehaviour)
        case p @ Patch(0, _, _) => (lower && lt(p.incrementMinor)).matches(that, preBehaviour)
        case p @ Patch(_, _, _) => (lower && lt(p.incrementMajor)).matches(that, preBehaviour)
        // ^0.0.3-beta := >=0.0.3-beta <0.0.4
        case p @ Pre(0, 0, _, _) => (lower && lt(p.incrementPatch)).matches(that, preBehaviour)
        // ^1.2.3-beta.2 := >=1.2.3-beta.2 <2.0.0 //1.2.3-beta.4 would be allowed, but 1.2.4-beta.2 would not
        case p @ Pre(0, _, _, _) => (lower && lt(p.incrementMinor)).matches(that, preBehaviour)
        case p @ Pre(_, _, _, _) => (lower && lt(p.incrementMajor)).matches(that, preBehaviour)
      }
    }
  }

  /** Tilde ranges: Flexible patch
    */
  case class Tilde(p: Partial) extends Simple {
    def matches(that: Version): Boolean = matches(that, Strict)
    def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean = {
      val lower = GTE(p)
      p match {
        case Wild => true // ~* kinda weird, but whatever
        case Major(Long.MaxValue) | Minor(Long.MaxValue, Long.MaxValue) |
            Patch(Long.MaxValue, Long.MaxValue, _) | Pre(Long.MaxValue, Long.MaxValue, _, _) =>
          lower.matches(that, preBehaviour)
        case m @ Major(_)    => (lower && lt(m.increment)).matches(that, preBehaviour) // same as ^
        case m @ Minor(_, _) => (lower && lt(m.increment)).matches(that, preBehaviour)
        case p @ Patch(_, _, _) => (lower && lt(p.incrementMinor)).matches(that, preBehaviour)
        // ~1.2.3-beta.2: >=1.2.3-beta.2 <1.3.0
        // 1.2.3-beta.4 would be allowed, but 1.2.4-beta.2 would not
        case p @ Pre(_, _, _, _) => (lower && LT(p.incrementMinor)).matches(that, preBehaviour)
      }
    }
  }

  /** Conjunction: Matches if all simple constituents match
    *
    * For a strict match all constituents must match loosely, and any of them strictly with regards
    * to pre-release behaviour.
    */
  case class And(simples: List[Simple]) extends Matcher {
    def matches(that: Version): Boolean = matches(that, Strict)
    def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean = {
      // consider >1.2.3-pre1 <=1.3 which should be able to match 1.2.3-pre2
      // but won't match <=1.3 strict, so we match all lose, and at least one strict
      simples.map(s => s.matches(that, Loose)).reduceLeft(_ && _) &&
      simples.map(s => s.matches(that, preBehaviour)).reduceLeft(_ || _)
    }
  }

  /** Disjunction: Matches if any of the constituents match
    */
  case class Or(ands: List[And]) extends Matcher {
    def matches(that: Version): Boolean = matches(that, Strict)
    def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean = {
      ands.map(s => s.matches(that, preBehaviour)).reduceLeft(_ || _)
    }
  }

  sealed trait Primitive extends Simple {
    def p: Partial
  }
  case class Exact(p: Partial) extends Primitive {
    override def matches(that: Version) = matches(that, Strict)
    override def matches(that: Version, preBehaviour: PreReleaseBehaviour) = p match {
      case Major(m) => that.major == m && (that.pre.isEmpty || preBehaviour == Loose)
      case Minor(maj, min) =>
        that.major == maj && that.minor == min && (that.pre.isEmpty || preBehaviour == Loose)
      case Patch(maj, min, pat) =>
        that.major == maj && that.minor == min && that.patch == pat && (that.pre.isEmpty || preBehaviour == Loose || preBehaviour == Strict)
      case Pre(maj, min, pat, pre) =>
        val coreMatches = (that.major == maj && that.minor == min && that.patch == pat)
        val preMatches  = preBehaviour != Never && pre == that.pre
        coreMatches && preMatches
      case Wild => true
    }
  }

  private def preOK(p: Partial, preBehaviour: PreReleaseBehaviour, v: Version) = {
    def onSamePatch = p match {
      case Patch(v.major, v.minor, v.patch) | Pre(v.major, v.minor, v.patch, _) => true
      case _                                                                    => false
    }

    v.pre.isEmpty ||
    preBehaviour == PreReleaseBehaviour.Loose ||
    preBehaviour == PreReleaseBehaviour.Strict && onSamePatch
  }

  case class GT(p: Partial) extends Primitive {
    implicit val preReleaseOrder: Ordering[SemVer.PreReleaseSuffix] =
      VersionOrder.preReleaseOrder

    override def matches(that: Version) = matches(that, Strict)
    override def matches(that: Version, preBehaviour: PreReleaseBehaviour) = {
      def versionOK = p match {
        case Wild                => true
        case Major(major)        => that.major > major
        case Minor(major, minor) => that.major > major || that.major == major && that.minor > minor
        case Patch(major, minor, patch) =>
          that.major > major || that.major == major && that.minor > minor || that.major == major && that.minor == minor && that.patch > patch
        case Pre(_, _, _, _) => that > p.version
      }

      preOK(p, preBehaviour, that) && versionOK
    }
  }

  case class GTE(p: Partial) extends Primitive {
    implicit val preReleaseOrder: Ordering[SemVer.PreReleaseSuffix] =
      VersionOrder.preReleaseOrder
    override def matches(that: Version) = matches(that, Strict)
    override def matches(that: Version, preBehaviour: PreReleaseBehaviour) = {
      def versionOK = p match {
        case Wild                => true
        case Major(major)        => that.major >= major
        case Minor(major, minor) => that.major > major || that.major == major && that.minor >= minor
        case Patch(major, minor, patch) =>
          that.major > major || that.major == major && that.minor > minor || that.major == major && that.minor == minor && that.patch > patch ||
            that.major == major && that.minor == minor && that.patch == patch && that.pre.isEmpty
        case Pre(_, _, _, _) => that >= p.version
      }

      preOK(p, preBehaviour, that) && versionOK
    }
  }

  case class LT(p: Partial) extends Primitive {
    implicit val preReleaseOrder: Ordering[SemVer.PreReleaseSuffix] =
      VersionOrder.preReleaseOrder

    override def matches(that: Version) = matches(that, Strict)
    override def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean =
      if (that.pre.isEmpty) that < p.version
      else if (preBehaviour == Never) false
      else
        p match {
          case Wild => true
          case Pre(major, minor, patch, pre) =>
            CoreVersion(major, minor, patch).get == that.coreVersion && that.pre < pre ||
              (that.coreVersion.toVersion < CoreVersion(major, minor, patch).get.toVersion) &&
              preBehaviour == Loose
          case _ if preBehaviour == Strict =>
            false
          case Major(major) =>
            that.major < major // <1 doesn't match 1.0.0-pre1 on either strict or loose
          case Minor(major, minor) =>
            that.major < major || that.major == major && that.minor < minor
          case Patch(major, minor, patch) =>
            that.major < major || that.major == major && (that.minor < minor || that.minor == minor && (that.patch < patch))
        }
  }

  case class LTE(p: Partial) extends Primitive {
    override def matches(that: Version) = matches(that, Strict)
    override def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean = {
      val versionOK = p match {
        case Wild                => true
        case Major(major)        => that.major <= major
        case Minor(major, minor) => that.major < major || that.major == major && that.minor <= minor
        case Patch(major, minor, patch) =>
          that.major < major ||
            that.major == major && that.minor < minor ||
            that.major == major && that.minor == minor && that.patch <= patch
        case Pre(_, _, _, _) => that <= p.version
      }

      preOK(p, preBehaviour, that) && versionOK
    }
  }

  /** a matches that matches if the given version is greater than this partial
    */
  def gt(p: Partial) = GT(p)

  /** a matcher that matches if the given version is greater or equal to this partial
    */
  def gte(p: Partial) = GTE(p)

  /** a matcher that matches if the given version is less than this partial
    */
  def lt(p: Partial) = LT(p)

  /** a matcher that matches if the given version is less than or equal to this partial
    */
  def lte(p: Partial) = LTE(p)

  /** a matcher that matches if the given version is equal to this partial
    */
  def eqv(p: Partial) = Exact(p)

  implicit final class SimpleAndOps(val self: Simple) extends AnyVal {
    def &&(that: Simple) = And(List(self, that))
    def &&(that: And)    = And(self :: that.simples)
  }

  implicit final class AndAndOps(val self: And) extends AnyVal {
    def &&(that: Simple) = And(that :: self.simples)
    def &&(that: And)    = And(that.simples ::: self.simples)
  }

  implicit final class OrOps(val self: Matcher) extends AnyVal {
    def ||(that: Matcher) = {
      def ands(m: Matcher) = m match {
        case Or(ands)  => ands
        case a: And    => List(a)
        case s: Simple => List(And(List(s)))
      }
      Or(ands(self) ::: ands(that))
    }
  }

  /** The lower bound of the given matcher
    */
  def lowerBound(m: Matcher): Bound = m match {
    case Hyphen(lower, _) => lowerBound(GTE(lower))
    case Caret(p)         => Inclusive(p.version)
    case Tilde(p)         => Inclusive(p.version)
    case And(simples)     => simples.map(lowerBound).reduceLeft(maxLower)
    case Or(ands)         => ands.map(lowerBound).reduceLeft(minLower)
    case Exact(p)         => Inclusive(p.version)
    case GT(p: Pre)       => Exclusive(p.version)
    case GT(Wild)         => Exclusive(Version.unsafe(0, 0, 0))
    case GT(p) =>
      try {
        Inclusive(p.increment.version)
      } catch {
        case _: ArithmeticException => Exclusive(p.version)
      }
    case GTE(p) => Inclusive(p.version)
    case LT(_)  => Inclusive(Version.unsafe(0, 0, 0))
    case LTE(_) => Inclusive(Version.unsafe(0, 0, 0))
  }

  /** The upper bound of the given matcher
    */
  def upperBound(m: Matcher): Bound = m match {
    case Hyphen(_, upper) =>
      upper match {
        case Wild | Major(Long.MaxValue) | Minor(Long.MaxValue, Long.MaxValue) => Unbounded
        case m: Major => Exclusive(m.increment.version)
        case m: Minor => Exclusive(m.increment.version)
        case p: Patch => Inclusive(p.version)
        case p: Pre   => Inclusive(p.version)
      }
    case Caret(part) =>
      part match {
        case Wild | Major(Long.MaxValue) | Minor(Long.MaxValue, Long.MaxValue) |
            Patch(Long.MaxValue, Long.MaxValue, Long.MaxValue) =>
          Unbounded // ^* kinda weird, but whatever
        case m @ Major(_)       => Exclusive(m.increment.version)
        case m @ Minor(_, _)    => Exclusive(m.increment.version)
        case p @ Patch(0, 0, _) => Inclusive(p.version)
        case p @ Patch(0, _, _) => Exclusive(p.incrementMinor.version)
        case p @ Patch(_, _, _) => Exclusive(p.incrementMajor.version)
        // ^0.0.3-beta := >=0.0.3-beta <0.0.4 Note that prereleases in the 0.0.3 version only will be allowed, if they are greater than or equal to beta
        case pre @ Pre(0, 0, _, _) => Exclusive(pre.incrementPatch.version)
        // ^1.2.3-beta.2 := >=1.2.3-beta.2 <2.0.0 //1.2.3-beta.4 would be allowed, but 1.2.4-beta.2 would not
        case pre @ Pre(0, _, _, _) => Exclusive(pre.incrementMinor.version)
        case pre @ Pre(_, _, _, _) => Exclusive(pre.incrementMajor.version)
      }
    case Tilde(part) =>
      part match {
        case Wild | Major(Long.MaxValue) | Minor(Long.MaxValue, Long.MaxValue) |
            Patch(Long.MaxValue, Long.MaxValue, Long.MaxValue) =>
          Unbounded // ~* kinda weird, but whatever
        case m @ Major(_)        => Exclusive(m.increment.version)      // same as caret
        case m @ Minor(_, _)     => Exclusive(m.increment.version)      // in the glorious future
        case p @ Patch(_, _, _)  => Exclusive(p.incrementMinor.version) // binders should be
        case p @ Pre(_, _, _, _) => Exclusive(p.incrementMinor.version) // allowed in alternative
      }
    case Exact(p) => Exclusive(p.increment.version)
    case GT(_)    => Unbounded
    case GTE(_)   => Unbounded
    case LT(p)    => Exclusive(p.version)
    case LTE(p) =>
      p match {
        case Wild              => Unbounded
        case maj @ Major(_)    => Exclusive(maj.increment.version)
        case min @ Minor(_, _) => Exclusive(min.increment.version)
        case Patch(_, _, _)    => Inclusive(p.version)
        case Pre(_, _, _, _)   => Inclusive(p.version)
      }
    case And(simples) => simples.map(upperBound).reduceLeft(minUpper)
    case Or(ands)     => ands.map(upperBound).reduceLeft(maxUpper)
  }

  private def maxUpper(b1: Bound, b2: Bound): Bound = (b1, b2) match {
    case (Unbounded, _)                              => Unbounded
    case (_, Unbounded)                              => Unbounded
    case (b1: Bounded, b2: Bounded) if b1.by > b2.by => b1
    case (b1: Bounded, b2: Bounded) if b2.by > b1.by => b2
    case (i: Inclusive, _)                           => i
    case (_, o)                                      => o
  }
  private def maxLower(b1: Bound, b2: Bound): Bound = (b1, b2) match {
    case (Unbounded, x)                              => x
    case (x, Unbounded)                              => x
    case (b1: Bounded, b2: Bounded) if b1.by > b2.by => b1
    case (b1: Bounded, b2: Bounded) if b2.by > b1.by => b2
    case (i: Inclusive, _)                           => i
    case (_, o)                                      => o
  }
  private def minUpper(b1: Bound, b2: Bound): Bound = if (maxUpper(b1, b2) == b1) b2 else b1
  private def minLower(b1: Bound, b2: Bound): Bound = if (maxLower(b1, b2) == b1) b2 else b1

  def print(m: Matcher): String = m match {
    case And(simples)         => simples.toList.map(print).mkString(" ")
    case Or(ands)             => ands.toList.map(print).mkString(" || ")
    case Caret(p)             => s"^$p"
    case Tilde(p)             => s"~$p"
    case Hyphen(lower, upper) => s"$lower - $upper"
    case Exact(p)             => p.toString()
    case GT(p)                => s">$p"
    case GTE(p)               => s">=$p"
    case LT(p)                => s"<$p"
    case LTE(p)               => s"<=$p"
  }

}
