package semver4s.gen

import org.scalacheck.Gen
import NumberGen._
import semver4s._

object GenMatcher {

  val genOp = Gen.oneOf(">", "=", "<", ">=", "<=")
  val genNonNumIdChar = Gen.oneOf(
    Gen.const('-'),
    Gen.choose('A', 'Z'),
    Gen.choose('a', 'z')
  )
  val genIdChar = Gen.oneOf(
    Gen.const('-'),
    Gen.choose('A', 'Z'),
    Gen.choose('a', 'z'),
    Gen.choose('0', '9')
  )

  val genId = GenVersion.genPreId.map(_.fold(identity, _.toString()))

  val genPre = for {
    len <- smallBetween(1, 10)
    l   <- Gen.listOfN(len, genId)
  } yield l.mkString("-", ".", "")

  val genPartial = {
    val wild = Gen.oneOf("", ".x", ".X", ".*")
    val es   = Gen.const("")
    val genPatch = for {
      patch <- nonNegativeLong
      pre   <- Gen.oneOf(es, genPre)
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

  val genSemver: Gen[String] =
    for {
      major <- nonNegativeLong
      minor <- nonNegativeLong
      patch <- nonNegativeLong
      pre   <- Gen.oneOf(Gen.const(""), genPre)
    } yield s"$major.$minor.$patch$pre"

  val genPrimitive: Gen[String] = for {
    op  <- Gen.oneOf(">", "=", "<", ">=", "<=")
    ver <- genSemver
  } yield op + ver

  val genHyphenRange: Gen[String] = for {
    from <- genPartial
    to   <- genPartial if from != "" && to != ""
  } yield s"$from - $to"

  val genTildeRange: Gen[String] = genPartial.map("~" + _)
  val genCaretRange: Gen[String] = genPartial.map("^" + _)

  val genRange: Gen[String] = {
    val nonHyphenRangePart = Gen.oneOf(genPartial, genPrimitive, genTildeRange, genCaretRange)
    val nonHyphenRange = for {
      n <- smallBetween(1, 10)
      l <- Gen.listOfN(n, nonHyphenRangePart)
    } yield l.mkString(" ")
    Gen.oneOf(genHyphenRange, nonHyphenRange)
  }

  val genRangeSet: Gen[String] = for {
    n <- smallBetween(1, 10)
    l <- Gen.listOfN(n, genRange)
  } yield l.mkString(" || ")

  val genMatcher = GenMatcher.genRangeSet.map(s => parseMatcher(s).toOption.get)

}
