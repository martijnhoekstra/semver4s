package semver4s

import scala.quoted._
import cats.data.NonEmptyList

object Literal:
  inline def version(inline s: String) = ${versionImpl('{s})}
  inline def matcher(inline s: String) = ${matcherImpl('{s})}

  extension (inline sc: StringContext)
    inline def v(inline args: Any*) = ${interpolatedVersionImpl('{sc})}
    inline def m(inline args: Any*) = ${interpolatedMatcherImpl('{sc})}

  private def unliftLiteral[A: FromExpr, B: ToExpr](a: Expr[A], f: A => Option[B])(using Quotes): Expr[B] =
    Expr(f(a.valueOrError).getOrElse {
      quotes.reflect.report.error("value not in unlifted domain of f")
      ???
    })

  given ToExpr[Version] with
    def apply(x: Version)(using Quotes): Expr[Version] = x match {
      case Version(maj, min, pat, pre, bld) =>
        '{Version(${Expr(maj)}, ${Expr(min)}, ${Expr(pat)}, ${Expr(pre)}, ${Expr(bld)})}
    }

  given ToExpr[Matcher.Simple] with
    import Matcher._
    def apply(m: Simple)(using Quotes): Expr[Simple] = m match {
      case Hyphen(below, above) => '{Hyphen(${Expr(below)}, ${Expr(above)})}
      case Caret(p) => '{Caret(${Expr(p)})}
      case Tilde(p) => '{Tilde(${Expr(p)})}
      case Exact(p) =>'{Exact(${Expr(p)})}
      case GT(p) => '{GT(${Expr(p)})}
      case GTE(p) => '{GTE(${Expr(p)})}
      case LT(p) => '{LT(${Expr(p)})}
      case LTE(p) => '{LTE(${Expr(p)})}
    }

  given ToExpr[Matcher.And] with
    import Matcher._
    def apply(m: And)(using Quotes): Expr[And] = '{And(${Expr(m.simples)})}

  given ToExpr[Matcher.Or] with
    import Matcher._
    def apply(m: Or)(using Quotes): Expr[Or] = '{Or(${Expr(m.ands)})}

  given ToExpr[Matcher] with
    import Matcher._
    def apply(m: Matcher)(using Quotes): Expr[Matcher] = m match {
      case s: Simple => Expr(s)
      case o: Or => Expr(o)
      case a: And => Expr(a)
    }


  given ToExpr[Partial] with
    import Partial._
    def apply(x: Partial)(using Quotes): Expr[Partial] = x match {
      case Wild => '{Partial.Wild}
      case Major(m) => '{Partial.Major(${Expr(m)})}
      case Minor(maj, min) => '{Partial.Minor(${Expr(maj)}, ${Expr(min)})}
      case Patch(maj, min, pat) => '{Partial.Patch(${Expr(maj)}, ${Expr(min)}, ${Expr(pat)})}
      case Pre(maj, min, pat, pre) => '{Partial.Pre(${Expr(maj)}, ${Expr(min)}, ${Expr(pat)}, ${Expr(pre)})}
    }

  given [T: ToExpr : Type]: ToExpr[NonEmptyList[T]] with
    def apply(xs: NonEmptyList[T])(using Quotes) =
      '{NonEmptyList(${Expr(xs.head)}, ${Expr(xs.tail)})}

  private def versionImpl(expr: Expr[String])(using Quotes): Expr[Version] =
    unliftLiteral(expr, a => parsing.SemverParser.semver.parseAll(a).toOption)

  private def matcherImpl(expr: Expr[String])(using Quotes): Expr[Matcher] =
    unliftLiteral(expr, a => parsing.MatcherParser.matcher.parseAll(a).toOption)

  private def interpolatedVersionImpl(esc: Expr[StringContext])(using Quotes) = {
    val sc = esc.valueOrError
    sc.parts match
      case List(single: String) => versionImpl(Expr(single))
      case _ => {
        quotes.reflect.report.error("version interpolator allows only single string")
        ???
      }
  }

  private def interpolatedMatcherImpl(esc: Expr[StringContext])(using Quotes) = {
    val sc = esc.valueOrError
    sc.parts match
      case List(single: String) => matcherImpl(Expr(single))
      case _ => {
        quotes.reflect.report.error("matcher interpolator allows only single string")
        ???
      }
  }
