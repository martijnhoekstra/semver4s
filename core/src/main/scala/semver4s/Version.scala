package semver4s

object Version {
  def apply(major: Long, minor: Long, patch: Long): Option[Version] =
    if (major < 0 || minor < 0 || patch < 0) None
    else if (major == 0 && minor == 0 && patch == 0) None
    else Some(new Version(major, minor, patch, Nil, None) {})

  def apply(major: Long, minor: Long, patch: Long, pre: SemVer.PreReleaseSuffix): Option[Version] =
    if (major < 0 || minor < 0 || patch < 0) None
    else if (major == 0 && minor == 0 && patch == 0) None
    else Some(new Version(major, minor, patch, pre, None) {})

  /** Construct a core version from major.minor.patch
    */
  def unsafe(major: Long, minor: Long, patch: Long) =
    new Version(major, minor, patch, Nil, None) {}
  def unsafe(major: Long, minor: Long, patch: Long, pre: SemVer.PreReleaseSuffix) =
    new Version(major, minor, patch, pre, None) {}
  def unsafe(
      major: Long,
      minor: Long,
      patch: Long,
      pre: SemVer.PreReleaseSuffix,
      bld: Option[String]
  ) = new Version(major, minor, patch, pre, bld) {}

  /** Construct a core version from major.minor.patch if all are Int
    */
  def unsafe(major: Int, minor: Int, patch: Int) =
    new Version(major.toLong, minor.toLong, patch.toLong, Nil, None) {}

  /** Version order, where each higher version sorts after a lower version
    */
  implicit val precedence: Ordering[Version] = {
    implicit val pre = VersionOrder.preReleaseOrder
    Ordering.by((v: Version) => (v.major, v.minor, v.patch, v.pre))
  }

}

/** A semver version specification
  *
  * Consisting of a core version of major.minor.patch, potentially a pre-release suffix and
  * potentially build metadata
  */
sealed abstract case class Version(
    major: Long,
    minor: Long,
    patch: Long,
    pre: SemVer.PreReleaseSuffix,
    build: Option[String]
) {
  def asPartial: Partial = pre match {
    case Nil => Partial.unsafe(major, minor, patch)
    case p   => Partial.unsafe(major, minor, patch, p)
  }
  def coreVersion = new CoreVersion(major, minor, patch) {}

  /** Checks if this version must be compatible with that version according to SemVer.
    *
    * Two versions are compatible if they are identical, or if they have the same major version and
    * neither has a pre-release suffix
    *
    * @param that
    *   version to check compatibility with
    * @return
    *   `true` if this version should be compatible with that version.
    */
  def isCompatibleWith(that: Version): Boolean =
    compatibility.matches(that, PreReleaseBehaviour.Strict)

  /** Checks if this version must be compatible with that version using the "early semver" spec
    *
    * Two versions are compatible if they are identical, or if they have the same major version and
    * neither has a pre-release suffix, or they both have major version 0, they have the same minor
    * version and neither has a pre-release suffix
    *
    * @param that
    *   version to check compatibility with
    * @return
    *   `true` if this version should be compatible with that version.
    */
  def isEarlyCompatibleWith(that: Version): Boolean =
    earlyCompatibility.matches(that, PreReleaseBehaviour.Strict)

  /** A matcher that matches exactly those version that according to Early SemVer must be compatible
    * with this version.
    *
    * Early SemVer differs from SemVer in that two versions with major version 0 must be compatible
    * if their minor versions are equal.
    */
  def earlyCompatibility: Matcher =
    if (pre.nonEmpty) Matcher.Exact(this.asPartial)
    else if (major == 0) Matcher.Exact(Partial.unsafe(0, minor))
    else Matcher.Exact(Partial.unsafe(major))

  /** A matcher that matches exactly those versions that according to SemVer must be compatible with
    * this version.
    */
  def compatibility: Matcher =
    if (pre.nonEmpty || major == 0) Matcher.Exact(this.asPartial)
    else Matcher.Exact(Partial.unsafe(major))
  def incrementMajor = Version.unsafe(major + 1, 0, 0)
  def incrementMinor = Version.unsafe(major, minor + 1, 0)
  def incrementPatch = Version.unsafe(major, minor, patch + 1)
  def increment = {
    def inc(suffix: SemVer.PreReleaseSuffix): SemVer.PreReleaseSuffix = suffix.reverse match {
      case Nil              => List(Left("-"))
      case Right(l) :: tail => (Right(l + 1) :: tail).reverse
      case Left(s) :: tail  => (Left(s + "-") :: tail).reverse
    }
    pre match {
      case Nil    => incrementPatch
      case prefix => Version.unsafe(major, minor, patch, inc(prefix))
    }
  }

  /** The version, formatted in SemVer format
    */
  def format: String = {
    val preString = if (pre.isEmpty) "" else pre.map(_.merge.toString).mkString("-", ".", "")
    val bldString = build.map("+" + _).getOrElse("")
    s"$major.$minor.$patch$preString$bldString"
  }
}

sealed abstract case class CoreVersion(major: Long, minor: Long, patch: Long) {
  def toVersion = Version.unsafe(major, minor, patch)
}

object CoreVersion {
  def apply(major: Long, minor: Long, patch: Long) = if (
    (major >= 0 && minor >= 0 && patch >= 0) && (major, minor, patch) != ((0, 0, 0))
  )
    Some(new CoreVersion(major, minor, patch) {})
  else None
  def unsafe(major: Long, minor: Long, patch: Long) = new CoreVersion(major, minor, patch) {}
}
