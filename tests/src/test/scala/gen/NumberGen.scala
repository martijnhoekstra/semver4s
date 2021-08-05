package semver4s.gen

import org.scalacheck.Gen

object NumberGen {
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

  def numBelow(bound: Int) = {
    val z = Integer.numberOfLeadingZeros(bound)
    smallishInt(32 - z).map(math.abs).map(x => if (x > bound) x % bound else x)
  }

  def smallBetween(min: Int, max: Int) = numBelow((max - min) + 1).map(_ + min)

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

}
