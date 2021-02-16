package object semver4s {
  import parsing._

  val semverParser  = SemverParser.semver
  val matcherParser = MatcherParser.rangeSet

  def matcher(src: String) = matcherParser.parseAll(src)
  def version(src: String) = semverParser.parseAll(src)
}
