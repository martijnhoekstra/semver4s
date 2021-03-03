package semver4s.parsing

import cats.parse.{Parser => P}
import cats.data.NonEmptyList

object MatcherParser {
  import semver4s.Partial
  import semver4s.Partial._
  import semver4s.Matcher
  import semver4s.Matcher._
  import SemverParser._
  /*
  A version range is a set of comparators which specify versions that satisfy the range.
  A comparator is composed of an operator and a version. The set of primitive operators is:
    <  Less than
    <= Less than or equal to
    >  Greater than
    >= Greater than or equal to
    =  Equal. If no operator is specified, then equality is assumed.
              As a result, this operator is optional, but MAY be included.
   */

  object Op {
    sealed trait SimpleOp
    object LT  extends SimpleOp
    object LTE extends SimpleOp
    object GT  extends SimpleOp
    object GTE extends SimpleOp
    object EQ  extends SimpleOp
  }

  val partial: P[Partial] = {
    val star = P.charIn("xX*")
    val s    = star.as(Nil -> None)

    val patch = (long ~ preRelease.?)
    val minor = long ~ {
      val patOrStar = s.orElse(patch.map { case (p, pre) => List(p) -> pre })
      (dot *> patOrStar).?.map(_.getOrElse(Nil -> None))
    }
    val major = long ~ {
      val minOrStar = s.orElse(minor.map { case (min, (pat, pre)) => (min :: pat) -> pre })
      (dot *> minOrStar).?.map(_.getOrElse(Nil -> None))
    }

    star
      .as(Wild)
      .orElse(major.map {
        case (maj, (List(min, pat), Some(pre))) => Partial.unsafe(maj, min, pat, pre)
        case (maj, (List(min, pat), None))      => Partial.unsafe(maj, min, pat)
        case (maj, (List(min), _))              => Partial.unsafe(maj, min)
        case (maj, _)                           => Partial.unsafe(maj)
      })

  }

  val qualifier = preRelease.? ~ build.?
  val caret     = P.char('^') *> partial.map(caretRange)
  val tilde     = P.char('~') *> partial.map(tildeRange)
  val primitive = {
    val lt  = P.char('<').as(Op.LT)
    val lte = P.string("<=").backtrack.as(Op.LTE)
    val gt  = P.char('>').as(Op.GT)
    val gte = P.string(">=").backtrack.as(Op.GTE)
    val eq  = P.char('=').as(Op.EQ)
    (P.oneOf(List(lte, gte, gt, lt, eq)) ~ partial).map { case (op, part) =>
      primitiveRange(op, part)
    }
  }
  val simple: P[Simple] = P.oneOf(
    List(
      tilde,
      caret,
      primitive,
      partial.map(primitiveRange(Op.EQ, _))
    )
  )
  val hyphen = partial ~ (P.string(" - ") *> partial)

  val range: P[And] = P.oneOf(
    List(
      hyphen.backtrack
        .map { case (from, to) => hyphenRange(from, to) }
        .map(h => And(NonEmptyList(h, Nil))),
      simple.repSep(P.char(' ').backtrack).map(And(_))
    )
  )

  val logicalOr = P.string("||").surroundedBy(P.char(' ').rep)
  val rangeSet  = range.repSep(logicalOr).map(Or(_))

  val matcher: P[Matcher] = rangeSet

  def hyphenRange(lower: Partial, upper: Partial) = Hyphen(lower, upper)

  def tildeRange(part: Partial) = Tilde(part)

  def caretRange(p: Partial) = Caret(p)

  def primitiveRange(op: Op.SimpleOp, p: Partial) = op match {
    case Op.EQ  => Exact(p)
    case Op.GT  => Matcher.gt(p)
    case Op.GTE => Matcher.gte(p)
    case Op.LT  => Matcher.lt(p)
    case Op.LTE => Matcher.lte(p)
  }
}
