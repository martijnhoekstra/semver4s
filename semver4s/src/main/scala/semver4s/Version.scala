package semver4s

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import cats.kernel.Comparison._

object SemVer {
  type Identifier       = Either[String, Long]
  type PreReleaseSuffix = NonEmptyList[Identifier]
}

object VersionOrder {
  import SemVer._

  val identifierPrecedence: Order[Identifier] = Order.from {
    case (Left(n1), Left(n2)) => Order.compare(n1, n2)
    case (Right(str1), Right(str2)) =>
      Order.compare(str1, str2) //ASCIIbetical lexical ordering
    case (Left(_), Right(_)) => LessThan.toInt //numerical sorts before alphanumerical
    case (Right(_), Left(_)) => GreaterThan.toInt
  }

  def preReleasePrecedence(l1: List[Identifier], l2: List[Identifier]): Int = (l1, l2) match {
    case (Nil, Nil)                                               => EqualTo.toInt
    case (_, Nil)                                                 => GreaterThan.toInt
    case (Nil, _)                                                 => LessThan.toInt
    case (i1 :: lt, i2 :: rt) if identifierPrecedence.eqv(i1, i2) => preReleasePrecedence(lt, rt)
    case (i1 :: _, i2 :: _)                                       => Order.compare(i1, i2)
  }

  val preReleaseOrder: Order[Option[PreReleaseSuffix]] = Order.from {
    case (None, None)       => EqualTo.toInt
    case (Some(x), Some(y)) => preReleasePrecedence(x.toList, y.toList)
    case (None, _)          => GreaterThan.toInt
    case (_, None)          => LessThan.toInt
  }

}

object Version {
  def apply(major: Long, minor: Long, patch: Long) = new Version(major, minor, patch, None, None)
  def apply(major: Int, minor: Int, patch: Int) =
    new Version(major.toLong, minor.toLong, patch.toLong, None, None)

  val precedence: Order[Version] = {
    implicit val pre = VersionOrder.preReleaseOrder
    Order.by((v: Version) => (v.major, v.minor, v.patch, v.pre))
  }

  /** Increments the version number
    *
    * such that the returned number for version `v` is the lowest version that would match `>v`
    */
  def inc(v: Version): Version = v match {
    case Version(maj, min, pat, Some(pre), _) => Version(maj, min, pat, Some(inc(pre)), None)
    case Version(maj, min, pat, None, _)      => Version(maj, min, pat + 1)
  }

  def inc(suffix: SemVer.PreReleaseSuffix): SemVer.PreReleaseSuffix = suffix.reverse match {
    case NonEmptyList(Right(l), tail) => NonEmptyList(Right(l + 1), tail).reverse
    case NonEmptyList(Left(s), tail)  => NonEmptyList(Left(s + "0"), tail).reverse
  }
}

case class Version(
    major: Long,
    minor: Long,
    patch: Long,
    pre: Option[SemVer.PreReleaseSuffix],
    build: Option[String]
) {
  def coreVersion = CoreVersion(major, minor, patch)
  def format: String = {
    val preString =
      pre.map(p => p.map(_.fold(_.toString, _.toString)).mkString_("-", ".", "")).getOrElse("")
    val bldString = build.map("+" + _).getOrElse("")
    s"$major.$minor.$patch$preString$bldString"
  }
}

case class CoreVersion(major: Long, minor: Long, patch: Long)
