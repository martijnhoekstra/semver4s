package semver4s

import cats.syntax.all._

//import cats.kernel.Order
//import cats.kernel.Comparison._
import cats.data.NonEmptyList
import semver4s.PreReleaseBehaviour._
import cats.kernel.Comparison._
import cats.kernel.Order

sealed trait PreReleaseBehaviour

object PreReleaseBehaviour {
  case object Loose extends PreReleaseBehaviour

  /** Never match a pre-release version
    */
  case object Never extends PreReleaseBehaviour

  /** Match a pre-release version if the matchers version also
    * has a pre-release version
    */
  case object Strict extends PreReleaseBehaviour
}

sealed trait Matcher extends Product with Serializable {
  def matches(that: Version): Boolean
  def matches(that: Version, pre: PreReleaseBehaviour): Boolean
}

sealed trait Simple extends Matcher
case class Hyphen(lower: Partial, upper: Partial) extends Simple {
  def matches(version: Version): Boolean = matches(version, Strict)
  def matches(version: Version, pre: PreReleaseBehaviour): Boolean = {
    implicit val vOrd: Order[Version] = Version.precedence
    //implicit val preOrd = VersionOrder.preReleaseOrder.toOrdering
    val lowerVersion = lower.version
    val matchesUpper = upper match {
      case Wild                => true
      case Major(major)        => major >= version.major
      case Minor(major, minor) => (major, minor) >= ((version.major, version.minor))
      case Patch(major, minor, patch) =>
        (major, minor, patch) >= ((version.major, version.minor, version.patch))
      case Pre(major, minor, patch, _) =>
        (major, minor, patch) > (
          (
            version.major,
            version.minor,
            version.patch
          )
        ) //Some(pre)) >= ((version.major, version.minor, version.pre))
    }
    (lowerVersion <= version) && matchesUpper
  }
  /*
    val matchLower = Matcher.gte(lower match {
      case Wild                          => Version(0, 0, 0)
      case Major(major)                  => Version(major, 0, 0)
      case Minor(major, minor)           => Version(major, minor, 0)
      case Patch(major, minor, patch)    => Version(major, minor, patch)
      case Pre(major, minor, patch, pre) => Version(major, minor, patch, Some(pre), None)
    })
    upper match {
      case Wild                => matchLower
      case Major(major)        => matchLower && LT(Version(major + 1, 0, 0))
      case Minor(major, minor) => matchLower && LT(Version(major, minor + 1, 0))
      case Patch(major, minor, patch) =>
        matchLower && Matcher.lte(Version(major, minor, patch))
      case Pre(major, minor, patch, pre) =>
        matchLower && Matcher.lte(Version(major, minor, patch, Some(pre), None)) // <- ???
    } */
}
case class Caret(p: Partial) extends Simple {
  def matches(that: Version): Boolean = matches(that, Strict)
  def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean = {
    val lower = GTE(p)
    p match {
      case Wild                => true //^* kinda weird, but whatever
      case Major(m)            => (lower && LT(Major(m + 1))).matches(that, preBehaviour)
      case Minor(major, minor) => (lower && LT(Minor(major, minor + 1))).matches(that, preBehaviour)
      case Patch(0, 0, _)      => Matcher.eqv(p).matches(that, preBehaviour)
      case Patch(0, minor, _)  => (lower && LT(Minor(0, minor + 1))).matches(that, preBehaviour)
      case Patch(major, _, _)  => (lower && LT(Major(major))).matches(that, preBehaviour)
      //^0.0.3-beta := >=0.0.3-beta <0.0.4 Note that prereleases in the 0.0.3 version only will be allowed, if they are greater than or equal to beta
      case Pre(0, 0, pat, _) => (lower && LT(Patch(0, 0, pat + 1))).matches(that, preBehaviour)
      //^1.2.3-beta.2 := >=1.2.3-beta.2 <2.0.0 //1.2.3-beta.4 would be allowed, but 1.2.4-beta.2 would not
      case Pre(0, min, _, _) => (lower && LT(Minor(0, min + 1))).matches(that, preBehaviour)
      case Pre(maj, _, _, _) => (lower && LT(Major(maj + 1))).matches(that, preBehaviour)
    }
  }
}
case class Tilde(p: Partial) extends Simple {
  def matches(that: Version): Boolean = matches(that, Strict)
  def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean = {
    val lower = GTE(p)
    p match {
      case Wild                => true                                                        //~* kinda weird, but whatever
      case Major(major)        => (lower && LT(Major(major + 1))).matches(that, preBehaviour) //same as ^
      case Minor(major, minor) => (lower && LT(Minor(major, minor + 1))).matches(that, preBehaviour)
      case Patch(major, minor, _) =>
        (lower && LT(Minor(major, minor + 1))).matches(that, preBehaviour)
      //~1.2.3-beta.2: >=1.2.3-beta.2 <1.3.0
      //1.2.3-beta.4 would be allowed, but 1.2.4-beta.2 would not
      case Pre(major, minor, _, _) =>
        (lower && LT(Minor(major, minor + 1))).matches(that, preBehaviour)
    }
  }
}
case class And(simples: NonEmptyList[Simple]) extends Matcher {
  def matches(that: Version): Boolean = matches(that, Strict)
  def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean = {
    //consider >1.2.3-pre1 <=1.3 which should be able to match 1.2.3-pre2
    //but won't match <=1.3 strict, so we match all lose, and at least one strict
    simples.map(s => s.matches(that, Loose)).reduceLeft(_ && _) &&
    simples.map(s => s.matches(that, preBehaviour)).reduceLeft(_ || _)
  }
}
case class Or(ands: NonEmptyList[And]) extends Matcher {
  def matches(that: Version): Boolean = matches(that, Strict)
  def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean = {
    ands.map(s => s.matches(that, preBehaviour)).reduceLeft(_ || _)
  }
}

sealed trait Primitive extends Simple
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
      val preMatches  = preBehaviour != Never && Some(pre) == that.pre
      coreMatches && preMatches
    case Wild => true
  }

}

case class GT(p: Partial) extends Primitive {
  implicit val preReleaseOrder: Order[Option[SemVer.PreReleaseSuffix]] =
    VersionOrder.preReleaseOrder

  override def matches(that: Version) = matches(that, Strict)
  override def matches(that: Version, preBehaviour: PreReleaseBehaviour) = p match {
    case Wild         => that.pre.isEmpty || preBehaviour == Loose
    case Major(major) => that.major > major && (preBehaviour == Loose || that.pre.isEmpty)
    case Minor(major, minor) =>
      (that.major > major || that.major == major && that.minor > minor) &&
        (that.pre.isEmpty || preBehaviour == Loose)
    case Patch(major, minor, patch) => (
      (that.major > major ||
        that.major == major && that.minor > minor ||
        that.major == major && that.minor == minor && that.patch > patch) && (that.pre.isEmpty || preBehaviour == Loose)
    )
    case Pre(major, minor, pat, pre) =>
      preBehaviour match { // >1.2.3-alpha.1
        case Never => false
        case Strict => //matches 1.2.3-alpha.2 and 1.2.3
          that.major == major && that.minor == minor && that.patch == pat && that.pre > Option(pre)
        case Loose =>
          //matches 2.whatever
          that.major > major ||
            //matches 1.3.whatever
            that.major == major && that.minor > minor ||
            //matches 1.2.4(-whatever)
            that.major == major && that.minor == minor && that.patch > pat ||
            //matches 1.2.3 and 1.2.3-alpha.2
            that.major == major && that.minor == minor && that.patch == pat && that.pre > Option(
              pre
            )
      }
  }
}

case class GTE(p: Partial) extends Primitive {
  implicit val preReleaseOrder: Order[Option[SemVer.PreReleaseSuffix]] =
    VersionOrder.preReleaseOrder
  implicit val precedence: Order[Version] = Version.precedence

  override def matches(that: Version) = matches(that, Strict)
  override def matches(that: Version, preBehaviour: PreReleaseBehaviour) = p match {
    case Wild         => that.pre.isEmpty || preBehaviour == Loose
    case Major(major) => that.major >= major && (preBehaviour == Loose || that.pre.isEmpty)
    case Minor(major, minor) =>
      (that.major > major || that.major == major && that.minor >= minor) &&
        (that.pre.isEmpty || preBehaviour == Loose)
    case Patch(major, minor, patch) => (
      (that.major > major ||
        that.major == major && that.minor > minor ||
        that.major == major && that.minor == minor && that.patch >= patch) && (that.pre.isEmpty || preBehaviour == Loose)
    )
    case Pre(major, minor, pat, pre) =>
      preBehaviour match { // >1.2.3-alpha.1
        case Never => false
        case Strict => //matches 1.2.3-alpha.2 and 1.2.3
          that.major == major &&
            that.minor == minor &&
            that.patch == pat &&
            that.pre >= Option(pre)
        case Loose => that >= p.version
      }
  }
}

case class LT(p: Partial) extends Primitive {
  implicit val preReleaseOrder: Order[Option[SemVer.PreReleaseSuffix]] =
    VersionOrder.preReleaseOrder
  implicit val precedence: Order[Version] = Version.precedence

  override def matches(that: Version) = matches(that, Strict)
  override def matches(that: Version, preBehaviour: PreReleaseBehaviour): Boolean =
    if (that.pre.isEmpty) that < p.version
    else if (preBehaviour == Never) false
    else
      p match {
        case Wild => true
        case Pre(major, minor, patch, pre) =>
          CoreVersion(major, minor, patch) == that.coreVersion && that.pre < Option(pre)
        case _ if preBehaviour == Strict =>
          false
        case Major(major) =>
          that.major < major // <1 doesn't match 1.0.0-pre1 on either strict or loose
        case Minor(major, minor) => that.major < major || that.major == major && that.minor < minor
        case Patch(major, minor, patch) =>
          that.major < major || that.major == major && (that.minor < minor || that.minor == minor && (that.patch < patch))
      }
}

case class LTE(p: Partial) extends Primitive {
  override def matches(that: Version) = matches(that, Strict)
  override def matches(that: Version, pre: PreReleaseBehaviour): Boolean = {
    (p, pre) match {
      case (Wild, _)             => true
      case (Major(major), Loose) => that.major <= major
      case (Major(major), _)     => that.major <= major && that.pre.isEmpty
      case (Minor(major, minor), Loose) =>
        that.major < major ||
          that.major == major && that.minor <= minor
      case (Minor(major, minor), _) =>
        (
          that.major < major ||
            that.major == major && that.minor <= minor
        ) && that.pre.isEmpty
      case (Patch(major, minor, patch), Loose) => (
        that.major < major ||
          that.major == major && that.minor < minor ||
          that.major == major && that.minor == minor && that.patch <= patch
      )
      case (Patch(major, minor, patch), _) => (
        that.major < major ||
          that.major == major && that.minor < minor ||
          that.major == major && that.minor == minor && that.patch < patch && that.pre.isEmpty ||
          that.major == major && that.minor == minor && that.patch == patch && (that.pre.isEmpty || pre == Strict)
      )
      case (Pre(_, _, _, _), Never) => false
      case (Pre(major, minor, patch, pre), _) =>
        that.major == major && that.minor == minor && that.patch == patch && that.pre <= Some(pre)
    }
  }
}

object Matcher {

  def gt(p: Partial)  = GT(p)
  def gte(p: Partial) = GTE(p)
  def lt(p: Partial)  = LT(p)
  def lte(p: Partial) = LTE(p)
  def eqv(p: Partial) = Exact(p)

  implicit final class SimpleAndOps(val self: Simple) extends AnyVal {
    def &&(that: Simple) = And(NonEmptyList(self, List(that)))
    def &&(that: And)    = And(that.simples.prepend(self))
  }

  implicit final class AndAndOps(val self: And) extends AnyVal {
    def &&(that: Simple) = And(self.simples.prepend(that))
    def &&(that: And)    = And(that.simples.concatNel(self.simples))
  }

  implicit final class OrOps(val self: Matcher) extends AnyVal {
    def ||(that: Matcher) = {
      def ands(m: Matcher) = m match {
        case Or(ands)  => ands
        case a: And    => NonEmptyList.one(a)
        case s: Simple => NonEmptyList.one(And(NonEmptyList.one(s)))
      }
      Or(ands(self).concatNel(ands(that)))
    }
  }

  def lowerBound(m: Matcher): Bound = m match {
    case Hyphen(lower, _) => lowerBound(GT(lower))
    case Caret(p)         => Inclusive(p.version)
    case Tilde(p)         => Inclusive(p.version)
    case And(simples) =>
      simples
        .map(lowerBound)
        .reduceLeft((a, b) =>
          ((a, b): @unchecked) match {
            case (Inclusive(v1), Inclusive(v2)) => Inclusive(Version.precedence.max(v1, v2))
            case (Exclusive(v1), Exclusive(v2)) => Exclusive(Version.precedence.max(v1, v2))
            case (i @ Inclusive(v1), e @ Exclusive(v2)) =>
              Version.precedence.comparison(v1, v2) match {
                case GreaterThan => i
                case _           => e
              }
            case (e @ Exclusive(v1), i @ Inclusive(v2)) =>
              Version.precedence.comparison(v1, v2) match {
                case LessThan => i
                case _        => e
              }
          }
        )
    case Or(ands) =>
      ands
        .map(lowerBound)
        .reduceLeft((a, b) =>
          ((a, b): @unchecked) match {
            case (Inclusive(v1), Inclusive(v2)) => Inclusive(Version.precedence.min(v1, v2))
            case (Exclusive(v1), Exclusive(v2)) => Exclusive(Version.precedence.min(v1, v2))
            case (i @ Inclusive(v1), e @ Exclusive(v2)) =>
              Version.precedence.comparison(v1, v2) match {
                case LessThan => i
                case _        => e
              }
            case (e @ Exclusive(v1), i @ Inclusive(v2)) =>
              Version.precedence.comparison(v1, v2) match {
                case GreaterThan => i
                case _           => e
              }
          }
        )
    case Exact(p)   => Inclusive(p.version)
    case GT(p: Pre) => Exclusive(p.version)
    case GT(p)      => Inclusive(p.increment.version)
    case GTE(p)     => Inclusive(p.version)
    case LT(_)      => Inclusive(Version(0, 0, 0))
    case LTE(_)     => Inclusive(Version(0, 0, 0))
  }

  def upperBound(m: Matcher): Bound = ???

}
