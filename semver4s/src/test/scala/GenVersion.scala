package semver4s

import org.scalacheck.Gen
import cats.syntax.all._
import cats.data.NonEmptyList

object GenVersion {

  val smallishLong = {
    val unmaskBits = Gen.chooseNum(0, 64)
    val masks      = unmaskBits.map(-1L >>> _)
    for {
      mask <- masks
      num  <- Gen.choose(Long.MinValue, 0L)
      flip <- Gen.oneOf(-1, 1)
    } yield flip * (mask & num)
  }

  val smallishInt = {
    val unmaskBits = Gen.chooseNum(0, 32)
    val masks      = unmaskBits.map(-1 >>> _)
    for {
      mask <- masks
      num  <- Gen.choose(Int.MinValue, 0)
      flip <- Gen.oneOf(-1, 1)
    } yield flip * (mask & num)
  }

  val smallishNNLong = smallishLong.map(math.abs)
  val smallishNNInt  = smallishInt.map(math.abs)

  def numBetween(min: Long, max: Long, specials: Long*): Gen[Long] = {
    require(min <= max)
    if (min == max) Gen.const(min)
    else {
      val mid        = min + (max - min) / 2
      val deviations = Gen.oneOf(Gen.const(0L), smallishLong)
      val rough = for {
        base <- Gen.oneOf(min :: max :: mid :: specials.toList)
        d    <- deviations
      } yield base + d
      rough.retryUntil(r => r >= min && r <= max)
    }
  }

  def numBetween(min: Int, max: Int, specials: Int*): Gen[Int] = {
    require(min <= max)
    if (min == max) Gen.const(min)
    else {
      val mid        = min + (max - min) / 2
      val deviations = Gen.oneOf(Gen.const(0), smallishInt)
      val rough = for {
        base <- Gen.oneOf(min :: max :: mid :: specials.toList)
        d    <- deviations
      } yield base + d
      rough.retryUntil(r => r >= min && r <= max)
    }
  }

  val nonNegativeLong = numBetween(0L, Long.MaxValue)

  val genNumericId: Gen[SemVer.Identifier] = smallishNNLong.map(_.asRight[String])
  val genAlphaId: Gen[SemVer.Identifier] = numBetween(1, 255).flatMap { length =>
    Gen
      .stringOfN(length, GenMatcher.genIdChar)
      .retryUntil(str => str.exists(ch => !ch.isDigit))
      .map(_.asLeft[Long])
  }

  val genPreId: Gen[SemVer.Identifier] = Gen.oneOf(genNumericId, genAlphaId)
  val genPre: Gen[SemVer.PreReleaseSuffix] =
    Gen.nonEmptyListOf(genPreId).map(NonEmptyList.fromListUnsafe)

  val genCoreVersion: Gen[CoreVersion] = for {
    major <- nonNegativeLong
    minor <- nonNegativeLong
    patch <- nonNegativeLong if (major, minor, patch) != ((0L, 0L, 0L))
  } yield CoreVersion(major, minor, patch)

  val genVersion: Gen[Version] = for {
    core <- genCoreVersion
    pre  <- Gen.option(genPre)
  } yield {
    val withPre = core.toVersion.copy(pre = pre)
    if (withPre.format.length > 300) core.toVersion else withPre
  }
}
