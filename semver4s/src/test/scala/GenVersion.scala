package semver4s

import org.scalacheck.Gen
import cats.syntax.all._
import cats.data.NonEmptyList

object GenVersion {

  def smallishLong(maxbits: Int = 64) = {
    val unmaskBits = Gen.choose(0, maxbits)
    val masks      = unmaskBits.map(bits => ~(-1L << bits))
    for {
      mask <- masks
      num  <- Gen.choose(Long.MinValue, 0L)
      flip <- Gen.oneOf(-1, 1)
    } yield flip * (mask & num)
  }

  def smallishInt(maxbits: Int = 32) = {
    val unmaskBits = Gen.choose(0, maxbits)
    val masks      = unmaskBits.map(bits => ~(-1 << bits))
    for {
      mask <- masks
      num  <- Gen.choose(Int.MinValue, 0)
      flip <- Gen.oneOf(-1, 1)
    } yield flip * (mask & num)
  }

  val smallishNNLong = smallishLong(33).map(math.abs)
  val smallishNNInt  = smallishInt(12).map(math.abs)

  def numBetween(min: Long, max: Long, specials: Long*): Gen[Long] = {
    require(min <= max)
    if (min == max) Gen.const(min)
    else {
      val dif        = max - min
      val z          = Integer.numberOfLeadingZeros(dif.toInt)
      val mid        = min + dif / 2
      val deviations = smallishLong(64 - z)
      val rough1 = for {
        base <- Gen.oneOf(min :: max :: mid :: specials.filter(x => x > min && x < max).toList)
        d    <- deviations
      } yield {
        val rough = base + d
          if (rough > max) base - math.abs(d)
          else if (rough < max) base + math.abs(d)
          else rough
      }
      rough1.retryUntil(i => i >= min && i <= max)
    }
  }

  def numBetween(min: Int, max: Int, specials: Int*): Gen[Int] = {
    require(min <= max)
    if (min == max) Gen.const(min)
    else {
      val dif        = max - min
      val z          = Integer.numberOfLeadingZeros(dif)
      val mid        = min + dif / 2
      val deviations = smallishInt(32 - z)
      val rough1 = for {
        base <- Gen.oneOf(min :: max :: mid :: specials.filter(x => x > min && x < max).toList)
        d    <- deviations
      } yield {
        val rough = base + d
          if (rough > max) base - math.abs(d)
          else if (rough < max) base + math.abs(d)
          else rough
      }
      rough1.retryUntil(i => i >= min && i <= max)
    }
  }

  val nonNegativeLong = numBetween(0L, Long.MaxValue)

  val genNumericId: Gen[SemVer.Identifier] = smallishNNLong.map(_.asRight[String])
  val genAlphaId: Gen[SemVer.Identifier] = {
    def fixup(str: String): Gen[String] = for {
      i  <- numBetween(0, str.length - 1)
      ch <- GenMatcher.genNonNumIdChar
    } yield str.updated(i, ch)

    for {
      len     <- numBetween(1, 255)
      attempt <- Gen.stringOfN(len, GenMatcher.genIdChar)
      fixed   <- if (attempt.forall(_.isDigit)) fixup(attempt) else Gen.const(attempt)
    } yield fixed.asLeft[Long]
  }

  val genPreId: Gen[SemVer.Identifier] = Gen.oneOf(genNumericId, genAlphaId)
  val genPre: Gen[SemVer.PreReleaseSuffix] =
    Gen.nonEmptyListOf(genPreId).map(NonEmptyList.fromListUnsafe)

  val genCoreVersion: Gen[CoreVersion] = for {
    major <- nonNegativeLong
    minor <- nonNegativeLong
    patch <- nonNegativeLong if (major, minor, patch) != ((0L, 0L, 0L))
  } yield CoreVersion.unsafe(major, minor, patch)

  val genVersion: Gen[Version] = for {
    core @ CoreVersion(major, minor, patch) <- genCoreVersion
    pre                                     <- Gen.option(genPre)
  } yield {
    val withPre = Version.unsafe(major, minor, patch, pre, None)
    if (withPre.format.length > 300) core.toVersion else withPre
  }
}
