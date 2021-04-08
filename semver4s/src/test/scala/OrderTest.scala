package semver4s

import cats.data.NonEmptyList
import cats.kernel.Order
import cats.syntax.all._

import org.scalacheck.Prop.forAll

import gen.GenVersion._

class OrderTest extends munit.ScalaCheckSuite {
  implicit val preReleaseOrder: Order[Option[SemVer.PreReleaseSuffix]] =
    VersionOrder.preReleaseOrder

  test("No pre-release after pre-release") {
    //11.3: When major, minor, and patch are equal, a pre-release version
    //has lower precedence than a normal version:
    val pre: SemVer.PreReleaseSuffix = NonEmptyList.one(1L.asRight[String])
    assert(clue(Option(pre)) < clue(None))
  }

  property("Shorter pre-release before longer") {
    //11.4.4: A larger set of pre-release fields has a higher precedence than a
    //smaller set, if all of the preceding identifiers are equal.
    forAll(genPre, genPre)((pre, extra) => {
      assert(Option(clue(pre)) > Option(clue(pre.concatNel(extra))))
    })
  }

  property("numeric pre-release before alpha") {
    //11.4.3: Numeric identifiers always have lower precedence than non-numeric identifiers.
    forAll(genAlphaId, genNumericId)((alpha, num) => {
      val alphaId = Option(NonEmptyList.one(alpha))
      val numId   = Option(NonEmptyList.one(num))
      assert(clue(numId) < clue(alphaId))
    })
  }

  property("alpha-numeric pre-release with a lower leading character sorts before a higher one") {
    forAll(genAlphaId, genAlphaId)((alpha1, alpha2) => {
      val ch1  = alpha1.left.getOrElse(???)
      val ch2  = alpha1.left.getOrElse(???)
      val pre1 = Option(NonEmptyList.one(clue(alpha1)))
      val pre2 = Option(NonEmptyList.one(clue(alpha2)))
      if (ch1 < ch2) assert(pre1 < pre2)
      if (ch1 > ch2) assert(pre1 > pre2)
    })
  }

  test("alpha.3 sorts before rc1 and after RC1") {
    val a3    = Option(NonEmptyList.of("alpha".asLeft[Long], 3L.asRight[String]))
    val rc1   = Option(NonEmptyList.one("rc1".asLeft[Long]))
    val rc1up = Option(NonEmptyList.one("RC1".asLeft[Long]))
    assert(clue(a3) < clue(rc1))
    assert(clue(a3) > rc1up)
  }

}
