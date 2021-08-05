package semver4s

import cats.syntax.either._
import org.scalacheck.Prop.forAll

import gen.GenVersion._

class OrderTest extends munit.ScalaCheckSuite {
  implicit val preReleaseOrder: Ordering[SemVer.PreReleaseSuffix] =
    VersionOrder.preReleaseOrder
  import math.Ordering.Implicits.infixOrderingOps

  test("No pre-release after pre-release") {
    //11.3: When major, minor, and patch are equal, a pre-release version
    //has lower precedence than a normal version:
    val pre: SemVer.PreReleaseSuffix = List(Right(1L))
    assert(clue(pre) < clue(Nil))
  }

  property("Shorter pre-release before longer") {
    //11.4.4: A larger set of pre-release fields has a higher precedence than a
    //smaller set, if all of the preceding identifiers are equal.
    forAll(genPre, genPre)((pre, extra) => {
      assert(clue(pre) > clue(pre ::: extra))
    })
  }

  property("Lower numeric before higher numeric") {
    //you'd think it's obvious
    forAll(genNumericId, genNumericId)((num1, num2) => {
      assertEquals(List(num1) < List(num2), num1.toOption.get < num2.toOption.get)
      assertEquals(List(num1) > List(num2), num1.toOption.get > num2.toOption.get)
    })
  }

  property("numeric pre-release before alpha") {
    //11.4.3: Numeric identifiers always have lower precedence than non-numeric identifiers.
    forAll(genAlphaId, genNumericId)((alpha, num) => {
      val alphaId = List(alpha)
      val numId   = List(num)
      assert(clue(numId) < clue(alphaId))
    })
  }

  property("alpha-numeric pre-release with a lower leading character sorts before a higher one") {
    forAll(genAlphaId, genAlphaId)((alpha1, alpha2) => {
      val ch1  = alpha1.left.getOrElse(???)
      val ch2  = alpha1.left.getOrElse(???)
      val pre1 = List(clue(alpha1))
      val pre2 = List(clue(alpha2))
      if (ch1 < ch2) assert(pre1 < pre2)
      if (ch1 > ch2) assert(pre1 > pre2)
    })
  }

  test("alpha.3 sorts before alpha.7 and rc1 but after RC1") {
    val a3    = List("alpha".asLeft[Long], 3L.asRight[String])
    val a7    = List("alpha".asLeft[Long], 7L.asRight[String])
    val rc1   = List("rc1".asLeft[Long])
    val rc1up = List("RC1".asLeft[Long])
    assert(clue(a3) < clue(rc1))
    assert(clue(a3) > rc1up)
    assert(clue(a3) < clue(a7))
  }
}
