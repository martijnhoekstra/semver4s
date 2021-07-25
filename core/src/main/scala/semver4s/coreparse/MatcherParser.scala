package semver4s.coreparse

import semver4s._

object MatcherParser {
  import SharedParser._

  def parseMatcher(src: String): Either[String, Matcher] = {
    val orParts = src.split(""" \|\| """).toList
    orParts.traverse(parseRange).map(xs => Matcher.Or(xs))
  }

  def parseRange(src: String): Either[String, Matcher.And] =
    src.split(" - ").toList.map(parsePartial) match {
      case lower :: upper :: Nil =>
        for {
          l <- lower
          u <- upper
        } yield Matcher.And(List(Matcher.Hyphen(l, u)))
      case _ => src.split(' ').toList.traverse(parseSimple).map(Matcher.And(_))
    }

  def parseSimple(str: String) =
    if (str.startsWith("^")) parsePartial(str.substring(1)).map(Matcher.Caret(_))
    else if (str.startsWith("~")) parsePartial(str.substring(1)).map(Matcher.Tilde(_))
    else parsePartial(str).map(Matcher.Exact(_))

  def parsePartial(str: String): Either[String, Partial] = parseVersionLike(str).flatMap(versionLikeToPartial)

  def versionLikeToPartial(vl: VersionLike): Either[String, Partial] = {
    val opt = vl match {
      case VersionLike(List(major, minor, patch), Nil, _, false) => Partial(major, minor, patch)
      case VersionLike(List(major, minor, patch), pre, _, false) => Partial(major, minor, patch, pre)
      case VersionLike(List(major, minor), Nil, None, _) => Partial(major, minor)
      case VersionLike(List(major), Nil, None, _) => Partial(major)
      case VersionLike(Nil, Nil, None, _) => Option(Partial.Wild)
      case _ => None
    }
    opt.fold[Either[String, Partial]](Left("invalid partial version format"))(Right(_))
  }
}
