package semver4s.gen

import org.scalacheck.Gen
import cats.syntax.all._
import cats.data.NonEmptyList
import semver4s._

object GenVersion {
  import NumberGen._

  val genNumericId: Gen[SemVer.Identifier] = smallishNNLong.map(_.asRight[String])
  val genAlphaId: Gen[SemVer.Identifier] = {
    def fixup(str: String): Gen[String] = for {
      i  <- numBetween(0, str.length - 1)
      ch <- GenMatcher.genNonNumIdChar
    } yield str.updated(i, ch)

    for {
      len     <- smallBetween(1, 255)
      attempt <- Gen.stringOfN(len, GenMatcher.genIdChar)
      fixed   <- if (attempt.forall(_.isDigit)) fixup(attempt) else Gen.const(attempt)
    } yield fixed.asLeft[Long]
  }

  val genPreId: Gen[SemVer.Identifier] = Gen.oneOf(genNumericId, genAlphaId)
  val genPre: Gen[SemVer.PreReleaseSuffix] = for {
    parts <- smallBetween(1, 5)
    list  <- Gen.listOfN(parts, genPreId)
  } yield NonEmptyList.fromListUnsafe(list)

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

  val genPartial = {
    val genMajor = smallishLong().map(l => Partial(l)).filter(_.nonEmpty).map(_.get)
    val genMinor = (for {
      maj <- smallishLong()
      min <- smallishLong()
    } yield Partial(maj, min)).filter(_.nonEmpty).map(_.get)
    val genPatch = (for {
      maj <- smallishLong()
      min <- smallishLong()
      pat <- smallishLong()
    } yield Partial(maj, min, pat)).filter(_.nonEmpty).map(_.get)
    val pre = (for {
      maj <- smallishLong()
      min <- smallishLong()
      pat <- smallishLong()
      pr  <- GenVersion.genPre
    } yield Partial(maj, min, pat, pr)).filter(_.nonEmpty).map(_.get)
    Gen.oneOf(Gen.const(Partial.Wild), genMajor, genMinor, genPatch, pre)
  }
}
