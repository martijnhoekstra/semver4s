package object semver4s {
  val semVer        = SemverParsers.semver
  val semVerMatcher = RangeParsers.rangeSet

  def matcher(src: String) = RangeParsers.rangeSet.parseAll(src)
  def version(src: String) = SemverParsers.semver.parseAll(src)
}
