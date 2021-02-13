package semver4s

import org.scalacheck.Gen
import cats.syntax.all._
import cats.data.NonEmptyList

object GenVersion {
  val genNumericId: Gen[SemVer.Identifier] = Gen.chooseNum(0L, Long.MaxValue).map(_.asRight[String])
  val genAlphaId: Gen[SemVer.Identifier] = Gen.stringOf(GenMatcher.genIdChar).filterNot(_.forall(_.isDigit)).map(_.asLeft[Long])

  val genPreId: Gen[SemVer.Identifier] =  Gen.oneOf(genNumericId, genAlphaId)
  val genPre: Gen[SemVer.PreReleaseSuffix] = Gen.nonEmptyListOf(genPreId).map(NonEmptyList.fromListUnsafe)

}