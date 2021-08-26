package semver4s.coreparse
import scala.util.Try
import semver4s._

object SharedParser {

  implicit final class IWishIHadTraverse[A](val as: List[A]) extends AnyVal {
    def traverse[L, R](f: A => Either[L, R]): Either[L, List[R]] = as
      .foldRight[Either[L, List[R]]](Right[L, List[R]](Nil))((a, agg) =>
        for {
          l <- agg
          r <- f(a)
        } yield (r :: l)
      )
  }

  def parsePrePart(prePart: String) = Try(prePart.toLong).toOption match {
    case Some(l) if prePart.forall(_ == '0') || l >= 1 => Right(Right(l))
    case _ => {
      val badIndex =
        prePart.indexWhere(ch => !(ch.toInt < 255 && (ch.isLetter || ch.isDigit || ch == '-')))
      if (badIndex >= 0) Left(s"bad character ${prePart(badIndex)}")
      else Right(Left(prePart))
    }
  }

  def parsePre(preString: String) = preString.split('.').toList.traverse(parsePrePart)

  def parseVersionNoPre(versionString: String): Either[String, (List[Long], Boolean)] = {
    val all = versionString.split('.').toList

    val (numbers, hasX) = all match {
      case init :+ last if Set("*", "x", "X").contains(last) => init -> true
      case _                                                 => all  -> false
    }

    numbers
      .traverse(parseLong)
      .map(ns => (ns, hasX))
  }

  def parseLong(str: String): Either[String, Long] =
    scala.util.Try(str.toLong).toEither.left.map(ex => ex.getMessage())

  def parseVersionNoBuild(versionString: String): Either[String, VersionLike] =
    versionString.indexOf('-') match {
      case -1 =>
        parseVersionNoPre(versionString).map { case (ns, x) => VersionLike(ns, Nil, None, x) }
      case n => {
        val (core, pre) = versionString.splitAt(n)
        for {
          t     <- parseVersionNoPre(core)
          preID <- parsePre(pre.substring(1))
        } yield VersionLike(t._1, preID, None, t._2)
      }
    }

  def parseVersionLike(versionString: String): Either[String, VersionLike] = {
    if (versionString == null) Left("null string is invalid")
    else if (versionString.trim().isEmpty) Left("blank string is invalid")
    else
      versionString.split('+').toList match {
        case Nil           => Left("invalid version")
        case List(noBuild) => parseVersionNoBuild(noBuild)
        case noBuild :: build :: Nil =>
          parseVersionNoBuild(noBuild).map(v => v.copy(bld = Some(build)))
        case _ => Left("`+` character is only allowed to introduce metadata")
      }
  }
}

case class VersionLike(
    numbers: List[Long],
    pre: SemVer.PreReleaseSuffix,
    bld: Option[String],
    hasX: Boolean
)
