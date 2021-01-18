package semver4s

import cats.syntax.all._

import cats.kernel.Order
import cats.kernel.Comparison.EqualTo
import cats.kernel.Comparison.GreaterThan
import cats.kernel.Comparison.LessThan

sealed trait Matcher {
  def matches(that: Version, includePre: Boolean = false): Boolean
}

case class Exact(v: Version) extends Matcher {
  override def matches(that: Version, includePre: Boolean = false) = v.coreVersion == that.coreVersion && v.pre == that.pre
}

case class GT(v: Version) extends Matcher {
  override def matches(that: Version, includePre: Boolean = false) =
    Order.by((x: Version) => (x.major, x.minor, x.patch)).comparison(that, v) match {
      case EqualTo     => VersionOrder.preReleaseOrder.gt(that.pre, v.pre)
      case GreaterThan => that.pre.isEmpty || includePre
      case LessThan    => false
    }
}

case class LT(v: Version) extends Matcher {
  override def matches(that: Version, includePre: Boolean = false): Boolean =
    Order.by((x: Version) => (x.major, x.minor, x.patch)).comparison(that, v) match {
      case EqualTo     => VersionOrder.preReleaseOrder.lt(that.pre, v.pre)
      case LessThan    => that.pre.isEmpty || includePre
      case GreaterThan => false
    }
}

case class Or(v1: Matcher, v2: Matcher) extends Matcher {
  override def matches(that: Version, includePre: Boolean): Boolean = v1.matches(that, includePre) || v2.matches(that, includePre)
}

case class And(v1: Matcher, v2: Matcher) extends Matcher {
  override def matches(that: Version, includePre: Boolean): Boolean = v1.matches(that, includePre) && v2.matches(that, includePre)
}

case object Always extends Matcher {
  override def matches(that: Version, IncludePre: Boolean): Boolean = true
}

object Matcher {
  def gte(v: Version) = Exact(v) || GT(v)
  def lte(v: Version) = Exact(v) || LT(v)

  implicit final class BooleanOps(val self: Matcher) extends AnyVal {
    def ||(that: Matcher) = Or(self, that)
    def &&(that: Matcher) = And(self, that)
  }

  /** Returns the lowest version that matches the given matcher
    *
    * or None if no version matches
    */
  def lowerBound(matcher: Matcher): Option[Version] = matcher match {
    case Exact(v)   => Some(v)
    case GT(v)      => Some(Version.inc(v))
    case LT(_)      => Some(Version(0, 0, 0))
    case Or(v1, v2) => (lowerBound(v1), lowerBound(v2)).mapN(Version.precedence.min)
    case And(v1, v2) =>
      for {
        min1 <- lowerBound(v1)
        min2 <- lowerBound(v2)
        both = Version.precedence.max(min1, min2) if v1.matches(both) && v2.matches(both)
      } yield both
    case Always => Some(Version(0, 0, 0))
  }

  def upperBound(matcher: Matcher): Option[Bound] = {
    implicit val boundOrder = new Order[Bound] {
      override def compare(x: Bound, y: Bound): Int = (x, y) match {
        case (Unbounded, Unbounded) => 0
        case (Unbounded, _)         => 1
        case (_, Unbounded)         => -1
        case (Inclusive(vinc), Exclusive(vex)) =>
          if (Version.precedence.gteqv(vinc, vex)) 1
          else if (Version.precedence.eqv(Version.inc(vinc), vex)) 0
          else -1
        case (Exclusive(vex), Inclusive(vinc)) =>
          if (Version.precedence.gteqv(vinc, vex)) -1
          else if (Version.precedence.eqv(Version.inc(vinc), vex)) 0
          else 1
        case (b1: Bounded, b2: Bounded) => Version.precedence.compare(b1.by, b2.by)
      }

    }
    matcher match {
      case Exact(v) => Some(Inclusive(v))
      case GT(_)    => Some(Unbounded)
      case Always   => Some(Unbounded)
      case LT(v) if v.pre.isEmpty && v.patch != 0 =>
        Some(Inclusive(Version(v.major, v.minor, v.patch - 1))) //This is a lie: 1.2.4-beta.1 - 1.2.4
      case LT(v)      => Some(Exclusive(v))
      case Or(v1, v2) => (upperBound(v1), upperBound(v2)).mapN(boundOrder.max)
      case And(v1, v2) =>
        for {
          max1 <- upperBound(v1)
          max2 <- upperBound(v2)
          result <- boundOrder.min(max1, max2) match {
            case Unbounded => Some(Unbounded)
            case ex @ Exclusive(by)
                if (max1 == ex && max2 == ex) ||
                  (max1 == ex && v2.matches(by)) ||
                  (max2 == ex && v1.matches(by)) =>
              Some(ex)
            case in @ Inclusive(by) if v1.matches(by) && v2.matches(by) => Some(in)
            case _                                                      => None
          }
        } yield result
    }
  }

}
