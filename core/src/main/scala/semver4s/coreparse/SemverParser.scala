package semver4s.coreparse

object SemverParser {
  import semver4s._

  def parseVersion(str: String) = SharedParser.parseVersionLike(str).flatMap(versionLikeToVersion)

  def versionLikeToVersion(vl: VersionLike): Either[String, Version] = {
    val opt = vl match {
      case VersionLike(List(major, minor, patch), Nil, None, _) => Version(major, minor, patch);
      case VersionLike(List(major, minor, patch), pre, None, _) => Version(major, minor, patch, pre);
      case VersionLike(List(major, minor, patch), pre, Some(bld), _) => Version(major, minor, patch, pre, bld);
      case _ => None
    }
    opt.fold[Either[String, Version]](Left(s"expected major, minor, patch, but got ${vl.numbers}"))(v => Right(v))
  }


}
