package semver4s.coreparse

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalacheck.Shrink
import semver4s._

class SemverCoreParserTest extends munit.ScalaCheckSuite {
  import coreparse.SemverParser

  val genPreReleaseExamples = Gen.oneOf("-alpha", "-beta", "-alpha.1", "-1", "-alpha.-1", "--0")
  val genMetadataExamples =
    Gen.oneOf("+001", "+20130313144700", "+exp.sha.5114f85", "+21AF26D3", "+21AF26D3--117B344092BD")

  val genSemVer: Gen[String] = for {
    major         <- Gen.choose(0, 1000)
    minor         <- Gen.choose(0, 1000)
    patch         <- Gen.choose(0, 10000)
    preRelease    <- Gen.oneOf(Gen.const(""), genPreReleaseExamples)
    buildMetadata <- Gen.oneOf(Gen.const(""), genMetadataExamples)
  } yield s"$major.$minor.$patch$preRelease$buildMetadata"

  test("Shared core version example") {
    val example = "298.549.1542"
    val byCore  = SharedParser.parseVersionNoPre(example)
    assert(byCore.isRight)
    val Right((numbers, hasX)) = byCore
    assert(!hasX)
    assertEquals(numbers(0), 298L)
    assertEquals(numbers(1), 549L)
    assertEquals(numbers(2), 1542L)
  }

  test("example version") {
    val example = "298.549.1542-beta"
    val byCore  = SemverParser.parseVersion(example)
    val byCats  = semver4s.parsing.SemverParser.semver.parseAll(example)
    assert(byCore.isRight)
    val Right(coreParsed) = byCore
    assertEquals(coreParsed.major, 298L)
    assertEquals(coreParsed.minor, 549L)
    assertEquals(coreParsed.patch, 1542L)
    assertEquals(coreParsed.pre, List(Left("beta")))
    assert(
      byCore == byCats,
      s"example string result different between core and cats parser: $byCore, $byCats"
    )
  }

  property("semver parses SemVer") {
    implicit val noShrink: Shrink[String] = Shrink.shrinkAny
    forAll(genSemVer) { (sv: String) =>
      {
        val parsed = SemverParser.parseVersion(sv)
        assert(parsed.isRight, s"parse failed: ${parsed.left.get}")
      }
    }
  }

  property("valid semver strings round-trip with format") {
    implicit val noShrink: Shrink[String] = Shrink.shrinkAny
    forAll(genSemVer) { (sv: String) =>
      {
        val Right(version) = SemverParser.parseVersion(sv)
        assertEquals(sv, version.format)
      }
    }
  }

  test("bld without pre parses") {
    val Right(parsed) = SemverParser.parseVersion("835.86.4341+21AF26D3")
    assertEquals(parsed.build, Some("21AF26D3"))
  }

  property("valid semver version round-trips with parse") {
    forAll(gen.GenVersion.genVersion) { (v: Version) =>
      {
        val str = v.format
        assertEquals(v.build.isDefined, str.contains('+'))
        val parse = SemverParser.parseVersion(str)
        assert(parse.isRight)
        val Right(parsed) = parse
        assertEquals(parsed, v)
      }
    }
  }

}
