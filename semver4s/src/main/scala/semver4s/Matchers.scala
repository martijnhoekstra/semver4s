package semver4s

import cats.syntax.all._

import cats.kernel.Order
import cats.kernel.Comparison.EqualTo
import cats.kernel.Comparison.GreaterThan
import cats.kernel.Comparison.LessThan

sealed trait PreReleaseBehaviour
object PreReleaseBehaviour {
  case object OnSamePatch extends PreReleaseBehaviour
  case object Always      extends PreReleaseBehaviour
  case object Never       extends PreReleaseBehaviour
  case object IfProvided  extends PreReleaseBehaviour
}
import PreReleaseBehaviour._

sealed trait Matcher { //alternatively, consider PreReleaseBehaviour as a property of Matcher
  def matches(that: Version): Boolean
  def matches(that: Version, pre: PreReleaseBehaviour): Boolean
}

case class Exact(v: Version) extends Matcher {
  override def matches(that: Version) = matches(that, IfProvided)
  override def matches(that: Version, pre: PreReleaseBehaviour) =
    (v.coreVersion == that.coreVersion) && (pre match {
      case OnSamePatch | PreReleaseBehaviour.Always | IfProvided => v.pre == that.pre
      case Never                                                 => v.pre.isEmpty && that.pre.isEmpty
    })
}

case class GT(v: Version) extends Matcher {
  override def matches(that: Version) = matches(that, IfProvided)
  override def matches(that: Version, pre: PreReleaseBehaviour) = {
    val coreDiff = Order.by((x: Version) => (x.major, x.minor, x.patch)).comparison(that, v)
    (coreDiff, pre) match {
      case (EqualTo, Never)                          => that.pre.isEmpty
      case (GreaterThan, (Never | OnSamePatch))      => that.pre.isEmpty
      case (EqualTo, _)                              => VersionOrder.preReleaseOrder.gt(that.pre, v.pre)
      case (GreaterThan, PreReleaseBehaviour.Always) => true
      case (GreaterThan, IfProvided)                 => that.pre.isEmpty
      case (LessThan, _)                             => false
    }
  }
}

case class LT(v: Version) extends Matcher {
  override def matches(that: Version) = matches(that, IfProvided)
  override def matches(that: Version, pre: PreReleaseBehaviour): Boolean = {
    val coreDiff = Order.by((x: Version) => (x.major, x.minor, x.patch)).comparison(that, v)
    (coreDiff, pre) match {
      case (EqualTo, Never)                               => false
      case (EqualTo, IfProvided) if v.pre.isEmpty         => false
      case (EqualTo, _)                                   => VersionOrder.preReleaseOrder.lt(that.pre, v.pre)
      case (LessThan, (Never | OnSamePatch | IfProvided)) => that.pre.isEmpty
      case (LessThan, PreReleaseBehaviour.Always)         => true
      case (GreaterThan, _)                               => false
    }
  }
}

case class Or(v1: Matcher, v2: Matcher) extends Matcher {
  override def matches(that: Version): Boolean = v1.matches(that) || v2.matches(that)
  override def matches(that: Version, pre: PreReleaseBehaviour): Boolean =
    v1.matches(that, pre) || v2.matches(that, pre)
}

case class And(v1: Matcher, v2: Matcher) extends Matcher {
  override def matches(that: Version): Boolean = v1.matches(that) && v2.matches(that)
  override def matches(that: Version, pre: PreReleaseBehaviour): Boolean =
    v1.matches(that, pre) && v2.matches(that, pre)
}

case object Always extends Matcher {
  override def matches(that: Version): Boolean                           = true
  override def matches(that: Version, pre: PreReleaseBehaviour): Boolean = true
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
        Some(
          Inclusive(Version(v.major, v.minor, v.patch - 1))
        ) //This is a lie: 1.2.4-beta.1 - 1.2.4
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
