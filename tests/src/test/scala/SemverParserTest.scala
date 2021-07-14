package semver4s

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

class SemverParserTest extends munit.ScalaCheckSuite {
  import parsing.SemverParser

  val genPreRelease = Gen.oneOf("-alpha", "-beta", "-alpha.1")
  val genMetadata =
    Gen.oneOf("+001", "+20130313144700", "+exp.sha.5114f85", "+21AF26D3", "+21AF26D3--117B344092BD")

  val genSemVer: Gen[String] = for {
    major         <- Gen.choose(0, 1000)
    minor         <- Gen.choose(0, 1000)
    patch         <- Gen.choose(0, 10000)
    preRelease    <- Gen.oneOf(Gen.const(""), genPreRelease)
    buildMetadata <- Gen.oneOf(Gen.const(""), genMetadata)
  } yield s"$major.$minor.$patch$preRelease$buildMetadata"

  property("semver parses prerelease") {
    forAll(genPreRelease) { (pre: String) =>
      {
        assertEquals(SemverParser.preRelease.string.parseAll(pre), Right(pre))
      }
    }
  }

  property(("semver parsers metadata")) {
    forAll(genMetadata) { (meta: String) =>
      {
        assertEquals(SemverParser.build.string.parseAll(meta), Right(meta))
      }
    }
  }

  property("semver parses SemVer") {
    forAll(genSemVer) { (sv: String) =>
      {
        assertEquals(SemverParser.semver.string.parseAll(sv), Right(sv))
      }
    }
  }

  property("valid semver strings round-trip with format") {
    forAll(genSemVer) { (sv: String) =>
      {
        val Right(version) = SemverParser.semver.parseAll(sv)
        assertEquals(sv, version.format)
      }
    }
  }

  property("valid semver version round-trips with parse") {
    forAll(gen.GenVersion.genVersion) { (v: Version) =>
      {
        val str   = v.format
        val parse = SemverParser.semver.parseAll(str)
        assert(parse.isRight)
        val Right(parsed) = parse
        assertEquals(v, parsed)
      }
    }
  }
}
