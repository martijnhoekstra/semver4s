package catsparse.reporting
package semver4s

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import cats.parse.Parser
import cats.data.NonEmptyList

class ReporterTest extends munit.ScalaCheckSuite {

  property("single caret points correctly") {
    val parser = Parser.char('o').rep0
    forAll(Gen.chooseNum(0, 200), Gen.chooseNum(0, 200))((prefix: Int, postfix: Int) => {
      val teststring = ("o" * prefix) + "x" + ("o" * postfix)
      val reporter   = new Reporter(teststring)

      parser.parseAll(teststring).swap.map(reporter.report(_)) match {
        case Right(NonEmptyList(rep @ ErrorReport(context, pos, _), Nil)) =>
          assertEquals(context.indexOf("x"), pos, clue((teststring, context, pos)))
          assertEquals(rep.caretLine.indexOf("^"), pos, clue((teststring, context, pos)))
        case Right(_) => assert(false, "expected single location error")
        case Left(_)  => assert(false, "parse succeeded unexpectedly")
      }
    })
  }

  test("bad report") {
    val reporter = new Reporter("different")
    val teststring = "12345678901234567890"
    val p = Parser.length(20) *> Parser.char('x')
    def report = p.parseAll(teststring).swap.map(reporter.report(_))
    val ex = intercept[IllegalArgumentException](report)
    assertEquals(ex.getMessage(), "bad offset for position. Is the error from the same source as passed to the reporter?")
  }

  test("semver example 1") {
    import cats.parse.SemVer._

    val teststring = "1.2/3"
    val caret      = "   ^"

    val expectations = NonEmptyList.one("character '.'")
    val reporter       = new Reporter(teststring)

    semver.parseAll(teststring).swap.map(reporter.report(_)) match {
      case Right(NonEmptyList(rep @ ErrorReport(context, _, errors), Nil)) =>
        assertEquals(teststring, context)
        assertEquals(rep.caretLine, caret)
        assertEquals(errors, expectations)
      case Right(_) => assert(false, "expected single location error")
      case Left(_)  => assert(false, "parse succeeded unexpectedly")
    }
  }

  test("semver example 2") {
    import cats.parse.SemVer._

    val teststring = "1.2.3-pre???wrong"
    val caret      = "         ^"

    val expectederrors = "Expected the end of the string"
    val reporter       = new Reporter(teststring)

    semver.parseAll(teststring).swap.map(reporter.report(_)) match {
      case Right(NonEmptyList(rep @ ErrorReport(context, _, _), Nil)) =>
        assertEquals(teststring, context)
        assertEquals(rep.caretLine, caret)
        val messageBlock = rep.messagesAsBlock()
        assertEquals(messageBlock, expectederrors)
      case Right(_) => assert(false, "expected single location error")
      case Left(_)  => assert(false, "parse succeeded unexpectedly")
    }
  }

  test("semver example short") {
    import cats.parse.SemVer._

    val contextWidth = 5
    val teststring   = "1.2.3-pre???wrong"
    val shownContext = "-pre?"
    val caret        = "    ^"

    val expectederrors = "Expected the end of the string"
    val reporter       = new Reporter(teststring)

    semver.parseAll(teststring).swap.map(reporter.report(_, contextWidth)) match {
      case Right(NonEmptyList(rep @ ErrorReport(context, _, _), Nil)) =>
        assertEquals(shownContext, context)
        assertEquals(rep.caretLine, caret)
        assertEquals(rep.messagesAsBlock(), expectederrors)
      case Right(_) => assert(false, "expected single location error")
      case Left(_)  => assert(false, "parse succeeded unexpectedly")
    }
  }

  test("semver example mid") {
    import cats.parse.SemVer._

    val contextWidth = 13
    val teststring   = "1.2.3-pre???wrong"
    val shownContext = "1.2.3-pre???w"
    val caret        = "         ^"

    val expectederrors = "Expected the end of the string"
    val reporter       = new Reporter(teststring)

    semver.parseAll(teststring).swap.map(reporter.report(_, contextWidth)) match {
      case Right(NonEmptyList(rep @ ErrorReport(context, _, _), Nil)) =>
        assertEquals(shownContext, context)
        assertEquals(rep.caretLine, caret)
        assertEquals(rep.messagesAsBlock(), expectederrors)
      case Right(_) => assert(false, "expected single location error")
      case Left(_)  => assert(false, "parse succeeded unexpectedly")
    }
  }

  test("one of two") {
    val p            = Parser.charIn('.', ',')
    val teststring   = "xxxx"
    val caret        = "^"
    val shownContext = teststring

    val expectederrors = """|Expected any of the following:
                            |character ',' Or
                            |character '.'""".stripMargin
    val reporter = new Reporter(teststring)

    p.parseAll(teststring).swap.map(reporter.report(_)) match {
      case Right(NonEmptyList(rep @ ErrorReport(context, _, _), Nil)) =>
        assertEquals(shownContext, context)
        assertEquals(rep.caretLine, caret)
        assertEquals(rep.messagesAsBlock(), expectederrors)
      case Right(_) => assert(false, "expected single location error")
      case Left(_)  => assert(false, "parse succeeded unexpectedly")
    }
  }

    test("one of two consecutive") {
    val p            = Parser.charIn('.', '/')
    val teststring   = "xxxx"
    val caret        = "^"
    val shownContext = teststring

    val expectederrors = "Expected character '.' or '/'"
    val reporter = new Reporter(teststring)

    p.parseAll(teststring).swap.map(reporter.report(_)) match {
      case Right(NonEmptyList(rep @ ErrorReport(context, _, _), Nil)) =>
        assertEquals(shownContext, context)
        assertEquals(rep.caretLine, caret)
        assertEquals(rep.messagesAsBlock(), expectederrors)
      case Right(_) => assert(false, "expected single location error")
      case Left(_)  => assert(false, "parse succeeded unexpectedly")
    }
  }

  test("one of range") {
    val p            = Parser.charIn('.', ',', '-')
    val teststring   = "xxxx"
    val caret        = "^"
    val shownContext = teststring

    val expectederrors = "Expected a character between ',' and '.'"
    val reporter       = new Reporter(teststring)

    p.parseAll(teststring).swap.map(reporter.report(_)) match {
      case Right(NonEmptyList(rep @ ErrorReport(context, _, _), Nil)) =>
        assertEquals(shownContext, context)
        assertEquals(rep.caretLine, caret)
        assertEquals(rep.messagesAsBlock(), expectederrors)
      case Right(_) => assert(false, "expected single location error")
      case Left(_)  => assert(false, "parse succeeded unexpectedly")
    }
  }

  test("one of range plus one") {
    val p            = Parser.charIn('.', ',', '-', '\u1234')
    val teststring   = "xxxx"
    val caret        = "^"
    val shownContext = teststring

    val uu = "u1234"
    val expectederrors = s"""|Expected any of the following:
                             |a character between ',' and '.' Or
                             |character letter 'ሴ', codepoint \\$uu""".stripMargin

    val reporter = new Reporter(teststring)

    p.parseAll(teststring).swap.map(reporter.report(_)) match {
      case Right(NonEmptyList(rep @ ErrorReport(context, _, _), Nil)) =>
        assertEquals(shownContext, context)
        assertEquals(rep.caretLine, caret)
        assertEquals(rep.messagesAsBlock(), expectederrors)
      case Right(_) => assert(false, "expected single location error")
      case Left(_)  => assert(false, "parse succeeded unexpectedly")
    }
  }
}
