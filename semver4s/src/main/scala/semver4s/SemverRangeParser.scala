package semver4s

import cats.parse.{Parser => P}

object RangeParsers {
  import SemverParsers._
  sealed trait Partial

  case object Wild extends Partial

  case class Major(major: Long) extends Partial

  case class Minor(major: Long, minor: Long) extends Partial

  case class Patch(major: Long, minor: Long, patch: Long) extends Partial

  case class Pre(major: Long, minor: Long, patch: Long, pre: SemVer.PreReleaseSuffix)
      extends Partial

  /*
  A version range is a set of comparators which specify versions that satisfy the range.
  A comparator is composed of an operator and a version. The set of primitive operators is:
    <  Less than
    <= Less than or equal to
    >  Greater than
    >= Greater than or equal to
    =  Equal. If no operator is specified, then equality is assumed, so this operator is optional, but MAY be included.
   */

  object Op {
    sealed trait SimpleOp
    object LT  extends SimpleOp
    object LTE extends SimpleOp
    object GT  extends SimpleOp
    object GTE extends SimpleOp
    object EQ  extends SimpleOp
  }

  val star = P.charIn("xX*")

  val partial: P[Partial] = {
    val s = star.as(Nil -> None)

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
        case (maj, (List(min, pat), Some(pre))) => Pre(maj, min, pat, pre)
        case (maj, (List(min, pat), None))      => Patch(maj, min, pat)
        case (maj, (List(min), _))              => Minor(maj, min)
        case (maj, _)                           => Major(maj)
      })

  }

  /*
range-set  ::= range ( logical-or range ) *
logical-or ::= ( ' ' ) * '||' ( ' ' ) *
range      ::= hyphen | simple ( ' ' simple ) * | ''
hyphen     ::= partial ' - ' partial
simple     ::= primitive | partial | tilde | caret
primitive  ::= ( '<' | '>' | '>=' | '<=' | '=' ) partial
partial    ::= xr ( '.' xr ( '.' xr qualifier ? )? )?
xr         ::= 'x' | 'X' | '*' | nr
nr         ::= '0' | ['1'-'9'] ( ['0'-'9'] ) *
tilde      ::= '~' partial
caret      ::= '^' partial
qualifier  ::= ( '-' pre )? ( '+' build )?
pre        ::= parts
build      ::= parts
parts      ::= part ( '.' part ) *
part       ::= nr | [-0-9A-Za-z]+
   */

  val qualifier = preRelease.? ~ build.?
  val caret     = P.char('^') *> partial.map(caretRange)
  val tilde     = P.char('~') *> partial.map(tildeRange)
  val lt        = P.char('<').as(Op.LT)
  val lte       = P.string("<=").backtrack.as(Op.LTE)
  val gt        = P.char('>').as(Op.GT)
  val opgte     = P.string(">=").backtrack.as(Op.GTE)
  val eq        = P.char('=').as(Op.EQ)
  val primitive = P.oneOf(List(lte, opgte, gt, lt, eq)) ~ semver
  val simple = P.oneOf(
    List(
      tilde,
      caret,
      primitive.map { case (op, part) => primitiveRange(op, part) },
      partial.map(xrange)
    )
  )
  val hyphen = partial ~ (P.string(" - ") *> partial)
  
  val range = P.oneOf(
    List(
      hyphen.backtrack.map { case (from, to) => hyphenRange(from, to) },
      simple.repSep(P.char(' ').backtrack).map(_.reduceLeft(_ && _))
    )
  )
  
  val logicalOr = P.string("||").surroundedBy(P.char(' ').rep)
  val rangeSet  = range.repSep(logicalOr).map(_.reduceLeft(_ || _)).orElse(P.pure(Always))

  def hyphenRange(lower: Partial, upper: Partial): Matcher = {
    val matchLower = Matcher.gte(lower match {
      case Wild                          => Version(0, 0, 0)
      case Major(major)                  => Version(major, 0, 0)
      case Minor(major, minor)           => Version(major, minor, 0)
      case Patch(major, minor, patch)    => Version(major, minor, patch)
      case Pre(major, minor, patch, pre) => Version(major, minor, patch, Some(pre), None)
    })
    upper match {
      case Wild                => matchLower
      case Major(major)        => matchLower && LT(Version(major + 1, 0, 0))
      case Minor(major, minor) => matchLower && LT(Version(major, minor + 1, 0))
      case Patch(major, minor, patch) =>
        matchLower && Matcher.lte(Version(major, minor, patch))
      case Pre(major, minor, patch, pre) =>
        matchLower && Matcher.lte(Version(major, minor, patch, Some(pre), None)) // <- ???
    }
  }
  def tildeRange(part: Partial): Matcher = {
    val (from, to) = part match {
      case Wild => throw new IllegalArgumentException("wildcard pattern invalid for tilde range")
      case Pre(maj, min, pat, pre) =>
        Version(maj, min, pat, Some(pre), None) -> Version(maj, min + 1, 0)
      case Patch(maj, min, pat) =>
        Version(maj, min, pat) -> Version(maj, min + 1, 0)
      case Minor(maj, min) =>
        Version(maj, min, 0) -> Version(maj, min + 1, 0)
      case Major(maj) =>
        Version(maj, 0, 0) -> Version(maj + 1, 0, 0)
    }
    Matcher.gte(from) && LT(to)
  }

  def caretRange(p: Partial): Matcher = {
    import Matcher.gte
    p match {
      case Wild            => throw new IllegalArgumentException("wildcard pattern invalid for caret range")
      case Major(maj)      => gte(Version(maj, 0, 0)) && LT(Version(maj + 1, 0, 0))
      case Minor(0, min)   => gte(Version(0, min, 0)) && LT(Version(0, min + 1, 0))
      case Minor(maj, min) => gte(Version(maj, min, 0)) && LT(Version(maj + 1, 0, 0))

      case Patch(0, 0, pat) =>
        gte(Version(0, 0, pat)) && LT(Version(0, 0, pat + 1))
      case Pre(0, 0, pat, pre) =>
        gte(Version(0, 0, pat, Some(pre), None)) && LT(Version(0, 0, pat + 1))
      case Patch(0, min, pat) =>
        gte(Version(0, min, pat)) && LT(Version(0, min + 1, 0))
      case Pre(0, min, pat, pre) =>
        gte(Version(0, min, pat, Some(pre), None)) && LT(Version(0, min + 1, 0))
      case Patch(maj, min, pat) =>
        gte(Version(maj, min, pat)) && LT(Version(maj + 1, 0, 0))
      case Pre(maj, min, pat, pre) =>
        gte(Version(maj, min, pat, Some(pre), None)) && LT(Version(maj + 1, 0, 0))
    }
  }

  def primitiveRange(op: Op.SimpleOp, v: Version): Matcher = op match {
    case Op.EQ  => Exact(v)
    case Op.GT  => GT(v)
    case Op.GTE => Matcher.gte(v)
    case Op.LT  => LT(v)
    case Op.LTE => Matcher.lte(v)
  }

  def xrange(p: Partial): Matcher = p match {
    case Wild         => Always
    case Major(major) => Matcher.gte(Version(major, 0, 0)) && LT(Version(major + 1, 0, 0))
    case Minor(major, minor) =>
      Matcher.gte(Version(major, minor, 0)) && LT(Version(major, minor + 1, 0))
    case Patch(major, minor, patch)    => Exact(Version(major, minor, patch))
    case Pre(major, minor, patch, pre) => Exact(Version(major, minor, patch, Some(pre), None))
  }
}
