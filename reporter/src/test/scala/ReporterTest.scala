package catsparse.reporting
package semver4s

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import cats.parse.Parser

class ReporterTest extends munit.ScalaCheckSuite {

  property("caret points correctly") {
    val parser = Parser.char('o').rep0
    forAll(Gen.chooseNum(0, 200), Gen.chooseNum(0, 200))((prefix: Int, postfix: Int) => {
      val teststring = ("o" * prefix) + "x" + ("o" * postfix)
      val reporter   = new Reporter(teststring)
      parser.parseAll(teststring).swap.map(reporter.report(_).toList) match {
        case Right(context :: pointer :: _) =>
          assertEquals(
            context.indexOf("x"),
            pointer.indexOf("^"),
            clue((teststring, context, pointer))
          )
        case Right(_) => assert(false, "fewer than 2 lines in report")
        case Left(_)  => assert(false, "parse succeeded unexpectedly")
      }
    })
  }
}
