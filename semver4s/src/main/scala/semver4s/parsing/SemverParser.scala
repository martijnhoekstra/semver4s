package semver4s.parsing

import cats.implicits._
import cats.parse.{Parser => P, Numbers}
import cats.data.NonEmptyList
import scala.util.control.NonFatal
import semver4s.CoreVersion
import semver4s.Version

object SemverParser {
  type PreIdentifier = Either[String, Long]
  val int       = Numbers.bigInt.collect { case bi if bi.isValidInt && bi >= 0 => bi.intValue }
  val long      = Numbers.bigInt.collect { case bi if bi.isValidLong && bi >= 0 => bi.longValue }
  val dot       = P.char('.')
  val letter    = P.charWhere(ch => ch.toInt < 255 && ch.isLetter) //a-zA-Z, none of that ழு or Я
  val nonDigit  = letter.orElse(P.char('-').as('-'))
  val identChar = nonDigit.orElse(Numbers.digit)

  val prereleaseIdentifier: P[PreIdentifier] =
    identChar.void.rep.string.map(t =>
      try {
        if (t.head.isDigit) {
          t.toLong.asRight[String]
        } else t.asLeft[Long]
      } catch {
        case NonFatal(_) => t.asLeft[Long]
      }
    )

  val build: P[String] = P.char('+') *> identChar.rep.repSep(dot).string

  val preRelease: P[NonEmptyList[PreIdentifier]] =
    P.char('-') *> prereleaseIdentifier.repSep(dot)

  val versionCore: P[CoreVersion] = (long, dot *> long, dot *> long).mapN(CoreVersion.apply)

  val semver: P[Version] =
    (P.charIn('v', '=').?).with1 *> (versionCore ~ preRelease.? ~ build.?).map {
      case ((CoreVersion(major, minor, patch), pre), bld) =>
        Version.unsafe(major, minor, patch, pre, bld)
    }

}
