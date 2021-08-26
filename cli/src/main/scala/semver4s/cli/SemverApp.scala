package semver4s.cli

import semver4s._

import cats.effect.IO
import cats.effect.ExitCode
import cats.parse.{Parser0, Parser}
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect.CommandIOApp

import semver4s.Bound._
import cats.data.NonEmptyList

object SemverApp
    extends CommandIOApp(
      name = "semVer",
      header = "Tools for Semantic Versioning"
    ) {

  override def main = inc.orElse(min).orElse(max)

  val version  = Opts.argument[Version](metavar = "version")
  val versions = Opts.argument[NonEmptyList[Version]](metavar = "versions")
  val range    = Opts.argument[Matcher](metavar = "range")

  val inc = Opts.subcommand("inc", "increment the patch version") {
    version.map { v =>
      IO(println(v.incrementPatch.format)).map(_ => ExitCode.Success)
    }
  }

  val min = Opts.subcommand(
    "min",
    "print the lowest version that satisfies some range. If a list of versions is given, the version must be from that list"
  ) {
    val toPrint = (range, versions.orNone).mapN {
      case (r, None) =>
        (Matcher.lowerBound(r) match {
          case Unbounded     => "0.0.0"
          case Inclusive(by) => by.format
          case Exclusive(by) => by.format + " (exclusive)"
        }).pure[Option]
      case (r, Some(vs)) =>
        vs.toList.sorted.collectFirst {
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
    val toPrint = (range, versions.orNone).mapN {
      case (r, None) =>
        (Matcher.upperBound(r) match {
          case Unbounded    => "unbounded"
          case Inclusive(v) => v.format
          case Exclusive(v) => v.format + " (exclusive)"
        }).pure[Option]
      case (r, Some(vs)) =>
        vs.toList.sorted.reverse.collectFirst {
          case v if r.matches(v) => v.format
        }
    }
    toPrint.map(
      _.traverse(text => IO(println(text)).as(ExitCode.Success)).map(_.getOrElse(ExitCode.Error))
    )
  }

  implicit def versionArgument: Argument[Version] = new ParserArgument(semverParser) {
    def defaultMetavar = "major.minor.patch(-pre)?(+bld)?"
  }
  implicit def versionsArgument: Argument[NonEmptyList[Version]] =
    new ParserArgument(semverParser.repSep(Parser.char(';'))) {
      def defaultMetavar = "major.minor.patch(-pre)?(+bld)?(;major.minor.patch(-pre)?(+bld)?)*"
    }

  implicit def rangeArgument: Argument[Matcher] = new ParserArgument[Matcher](matcherParser) {
    def defaultMetavar = "major.minor.patch(-pre)?(+bld)?"
  }
}

abstract class ParserArgument[A](p: Parser0[A]) extends Argument[A] {
  def read(input: String) = {
    def reporter = new catsparse.reporting.Reporter(input)
    p.parseAll(input).left.map(reporter.report(_).flatMap(errs => errs.expectations)).toValidated
  }
}
