package semver4s

import org.scalacheck.Gen

object GenMatcher {
  val genOp = Gen.oneOf(">", "=", "<", ">=", "<=")
  val genIdChar = Gen.oneOf(
    Gen.const('-'),
    Gen.choose('0', '9'),
    Gen.choose('A', 'Z'),
    Gen.choose('a', 'z')
  )

  val genId = Gen
    .oneOf(
      Gen.chooseNum(0, Long.MaxValue).map(_.toString()),
      Gen.stringOf(genIdChar).filterNot(_.isEmpty())
    )
    .map("-" + _)

  val genPre = Gen.nonEmptyListOf(genId).map(_.mkString("-", ".", ""))

  val genPartial = {
    val wild = Gen.oneOf("", ".x", ".X", ".*")
    val genPatch = for {
      patch <- Gen.chooseNum(0L, Long.MaxValue)
      pre   <- Gen.oneOf(Gen.const(""), genPre)
    } yield "." + patch + pre

    val genMinor = for {
      minor     <- Gen.chooseNum(0L, Long.MaxValue)
      patchPart <- Gen.oneOf(wild, genPatch)
    } yield "." + minor + patchPart

    for {
      major     <- Gen.chooseNum(0L, Long.MaxValue)
      minorPart <- Gen.oneOf(wild, genMinor)
    } yield major.toString() + minorPart
  }

  val genSemver =
    for {
      major <- Gen.chooseNum(0L, Long.MaxValue)
      minor <- Gen.chooseNum(0L, Long.MaxValue)
      patch <- Gen.chooseNum(0L, Long.MaxValue)
      pre   <- Gen.oneOf(Gen.const(""), genPre)
    } yield s"$major.$minor.$patch$pre"

  val genPrimitive = for {
    op  <- Gen.oneOf(">", "=", "<", ">=", "<=")
    ver <- genSemver
  } yield op + ver

  val genHyphenRange = for {
    from <- genPartial
    to   <- genPartial
  } yield s"$from - $to"

  val genTildeRange = genPartial.map("~" + _)
  val genCaretRange = genPartial.map("^" + _)

  val genRange = {
    val nonHyphenRangePart = Gen.oneOf(genPartial, genPrimitive, genTildeRange, genCaretRange)
    val nonHyphenRange = Gen.nonEmptyListOf(nonHyphenRangePart).map(_.mkString(" "))
    Gen.oneOf(genHyphenRange, nonHyphenRange)
  }

  val genRangeSet = Gen.nonEmptyListOf(genRange).map(_.mkString(" || "))

}