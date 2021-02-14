package semver4s

import cats._
import cats.implicits._
import cats.data.NonEmptyList

object SemVer {
  type Identifier       = Either[String, Long]
  type PreReleaseSuffix = NonEmptyList[Identifier]
}

object Version {

  /** Construct a core version from major.minor.patch
    */
  def apply(major: Long, minor: Long, patch: Long) = new Version(major, minor, patch, None, None)

  /** Construct a core version from major.minor.patch if all are Int
    */
  def apply(major: Int, minor: Int, patch: Int) =
    new Version(major.toLong, minor.toLong, patch.toLong, None, None)

  /** Version order, where each higher version sorts after a lower version
    */
  implicit val precedence: Order[Version] = {
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

  private[this] def inc(suffix: SemVer.PreReleaseSuffix): SemVer.PreReleaseSuffix =
    suffix.reverse match {
      case NonEmptyList(Right(l), tail) => NonEmptyList(Right(l + 1), tail).reverse
      case NonEmptyList(Left(s), tail)  => NonEmptyList(Left(s + "0"), tail).reverse
    }
}

/** A semver version specification
  *
  * Consisting of a core version of major.minor.patch, potentially a pre-release
  * suffix and potentially build metadata
  */
case class Version(
    major: Long,
    minor: Long,
    patch: Long,
    pre: Option[SemVer.PreReleaseSuffix],
    build: Option[String]
) {
  def coreVersion = CoreVersion(major, minor, patch)

  /** The version, formatted in SemVer format
    */
  def format: String = {
    val preString =
      pre.map(p => p.map(_.fold(_.toString, _.toString)).mkString_("-", ".", "")).getOrElse("")
    val bldString = build.map("+" + _).getOrElse("")
    s"$major.$minor.$patch$preString$bldString"
  }
}

case class CoreVersion(major: Long, minor: Long, patch: Long) {
  def toVersion = Version(major, minor, patch)
}
