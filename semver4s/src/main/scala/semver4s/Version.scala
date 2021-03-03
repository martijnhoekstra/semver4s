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
  def unsafe(major: Long, minor: Long, patch: Long) = new Version(major, minor, patch, None, None){}
  def unsafe(major: Long, minor: Long, patch: Long, pre: SemVer.PreReleaseSuffix) = new Version(major, minor, patch, Some(pre), None){}
  def unsafe(major: Long, minor: Long, patch: Long, pre: Option[SemVer.PreReleaseSuffix], bld: Option[String]) = new Version(major, minor, patch, pre, bld){}

  /** Construct a core version from major.minor.patch if all are Int
    */
  def unsafe(major: Int, minor: Int, patch: Int) =
    new Version(major.toLong, minor.toLong, patch.toLong, None, None){}

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
    case Version(maj, min, pat, Some(pre), _) => Version.unsafe(maj, min, pat, inc(pre))
    case Version(maj, min, pat, None, _)      => Version.unsafe(maj, min, pat + 1)
  }

  private[this] def inc(suffix: SemVer.PreReleaseSuffix): SemVer.PreReleaseSuffix =
    suffix.reverse match {
      case NonEmptyList(Right(l), tail) => NonEmptyList(Right(l + 1), tail).reverse
      case NonEmptyList(Left(s), tail)  => NonEmptyList(Left(s + "-"), tail).reverse
    }
}

/** A semver version specification
  *
  * Consisting of a core version of major.minor.patch, potentially a pre-release
  * suffix and potentially build metadata
  */
sealed abstract case class Version(
    major: Long,
    minor: Long,
    patch: Long,
    pre: Option[SemVer.PreReleaseSuffix],
    build: Option[String]
) {
  def coreVersion = CoreVersion(major, minor, patch)
  def incrementMajor = Version.unsafe(major + 1, 0, 0)
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
  def toVersion = Version.unsafe(major, minor, patch)
}
