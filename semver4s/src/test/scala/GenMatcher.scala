package semver4s

import org.scalacheck.Gen

object GenMatcher {
  import GenVersion.nonNegativeLong
  
  val genOp = Gen.oneOf(">", "=", "<", ">=", "<=")
  val genIdChar = Gen.oneOf(
    Gen.const('-'),
    Gen.choose('0', '9'),
    Gen.choose('A', 'Z'),
    Gen.choose('a', 'z')
  )

  val genId = GenVersion.genPreId.map(_.fold(identity, _.toString()))

  val genPre = Gen.nonEmptyListOf(genId).map(_.mkString("-", ".", ""))

  val genPartial = {
    val wild = Gen.oneOf("", ".x", ".X", ".*")
    val genPatch = for {
      patch <- nonNegativeLong
      pre   <- Gen.oneOf(Gen.const(""), genPre)
    } yield "." + patch + pre

    val genMinor = for {
      minor     <- nonNegativeLong
      patchPart <- Gen.oneOf(wild, genPatch)
    } yield "." + minor + patchPart

    for {
      major     <- nonNegativeLong
      minorPart <- Gen.oneOf(wild, genMinor)
    } yield major.toString() + minorPart
  }

  val genSemver =
    for {
      major <- nonNegativeLong
      minor <- nonNegativeLong
      patch <- nonNegativeLong
      pre   <- Gen.oneOf(Gen.const(""), genPre)
    } yield s"$major.$minor.$patch$pre"

  val genPrimitive = for {
    op  <- Gen.oneOf(">", "=", "<", ">=", "<=")
    ver <- genSemver
  } yield op + ver

  val genHyphenRange = for {
    from <- genPartial
    to   <- genPartial if from != "" && to != ""
  } yield s"$from - $to"

  val genTildeRange = genPartial.map("~" + _)
  val genCaretRange = genPartial.map("^" + _)

  val genRange = {
    val nonHyphenRangePart = Gen.oneOf(genPartial, genPrimitive, genTildeRange, genCaretRange)
    val nonHyphenRange     = Gen.nonEmptyListOf(nonHyphenRangePart).map(_.mkString(" "))
    Gen.oneOf(genHyphenRange, nonHyphenRange)
  }

  val genRangeSet = Gen.nonEmptyListOf(genRange).map(_.mkString(" || "))

}
