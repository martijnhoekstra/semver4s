package semver4s.cli

import cats.syntax.all._

import semver4s._

import cats.effect.IO
import cats.effect.ExitCode
import cats.parse.{Parser0, Parser}
import com.monovore.decline._
import com.monovore.decline.effect.CommandIOApp

object SemverApp
    extends CommandIOApp(
      name = "semVer",
      header = "Tools for Semantic Versioning"
    ) {

  override def main = inc.orElse(min).orElse(max)

  val version  = Opts.argument[Version](metavar = "version")
  val versions = Opts.argument[List[Version]](metavar = "versions")
  val range    = Opts.argument[Matcher](metavar = "range")

  val inc = Opts.subcommand("inc", "increment the version") {
    version.map { v =>
      IO(println(Version.inc(v).format)).map(_ => ExitCode.Success)
    }
  }

  val min = Opts.subcommand(
    "min",
    "print the lowest version that satisfies some range. If a list of versions is given, the version must be from that list"
  ) {
    val toPrint = (range, versions).mapN {
      case (r, Nil) =>
        (Matcher.lowerBound(r) match {
          case Unbounded     => "0.0.0"
          case Inclusive(by) => by.format
          case Exclusive(by) => by.format + " (exclusive)"
        }).pure[Option]
      case (r, vs) =>
        vs.sorted(Version.precedence.toOrdering).collectFirst {
          case v if r.matches(v) => v.format
        }
    }
    toPrint.map(
      _.traverse(text => IO(println(text)).as(ExitCode.Success)).map(_.getOrElse(ExitCode.Error))
    )
  }

  val max = Opts.subcommand(
    "max",
    "print the highest version that satisfies some range. If a list of versions is given, the version must be from that list"
  ) {
    val toPrint = (range, versions).mapN {
      case (r, Nil) =>
        (Matcher.upperBound(r) match {
          case Unbounded    => "unbounded"
          case Inclusive(v) => v.format
          case Exclusive(v) => v.format + " (exclusive)"
        }).pure[Option]
      case (r, vs) =>
        vs.sorted(Version.precedence.toOrdering).reverse.collectFirst {
          case v if r.matches(v) => v.format
        }
    }
    toPrint.map(
      _.traverse(text => IO(println(text)).as(ExitCode.Success)).map(_.getOrElse(ExitCode.Error))
    )
  }

  implicit def versionArgument: Argument[Version] = new ParserArgument(semVer) {
    def defaultMetavar = "major.minor.patch(-pre)?(+bld)?"
  }
  implicit def versionsArgument: Argument[List[Version]] =
    new ParserArgument(semVer.repSep0(Parser.char(';'))) {
      def defaultMetavar = "major.minor.patch(-pre)?(+bld)?(;major.minor.patch(-pre)?(+bld)?)*"
    }

  implicit def rangeArgument: Argument[Matcher] = new ParserArgument[Matcher](semVerMatcher) {
    def defaultMetavar = "major.minor.patch(-pre)?(+bld)?"
  }
}

abstract class ParserArgument[A](p: Parser0[A]) extends Argument[A] {
  def read(string: String) = p.parseAll(string).left.map(Reporter.report(_, string)).toValidated
}
