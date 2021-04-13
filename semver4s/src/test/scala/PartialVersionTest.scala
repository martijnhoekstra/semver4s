package semver4s

import Partial._
import org.scalacheck.Prop.forAll
import gen.GenVersion._
import org.scalacheck.Arbitrary

class PartialVersionTest extends munit.ScalaCheckSuite {

  property("partial version apply succeeds for all partial versions") {
    forAll(genPartial) { (p: Partial) =>
      {
        def check(v: Option[Partial]) = assertEquals(Option(p), v)
        p match {
          case Wild                    => ()
          case Major(m)                => check(Partial(m))
          case Minor(maj, min)         => check(Partial(maj, min))
          case Patch(maj, min, pat)    => check(Partial(maj, min, pat))
          case Pre(maj, min, pat, pre) => check(Partial(maj, min, pat, pre))
        }
      }
    }
  }

  property(
    "Version/CoreVersion/Partial apply are consistent and don't generate negative versions"
  ) {
    implicit val arb: Arbitrary[SemVer.PreReleaseSuffix] = Arbitrary(genPre)
    forAll { (maj: Long, min: Long, pat: Long, pre: SemVer.PreReleaseSuffix) =>
      {
        val version     = Version(maj, min, pat, pre)
        val versionCore = Version(maj, min, pat)
        val coreVersion = CoreVersion(maj, min, pat)

        if (maj == 0 && min == 0 && pat == 0)
          assert(versionCore.isEmpty)
        else
          assertEquals(versionCore.map(_.coreVersion), coreVersion)

        assertEquals(version.map(_.coreVersion), coreVersion)

        val vmaj = Partial(maj)
        val vmin = Partial(maj, min)
        val vpat = Partial(maj, min, pat)
        val vpre = Partial(maj, min, pat, pre)
        if (maj < 0) {
          assert(version.isEmpty)
          assert(versionCore.isEmpty)
          assert(coreVersion.isEmpty)
          assert(vmaj.isEmpty)
          assert(vmin.isEmpty)
          assert(vpat.isEmpty)
          assert(vpre.isEmpty)
        } else {
          assert(vmaj.isDefined)
          if (min < 0) {
            assert(version.isEmpty)
            assert(versionCore.isEmpty)
            assert(coreVersion.isEmpty)
            assert(vmin.isEmpty)
            assert(vpat.isEmpty)
            assert(vpre.isEmpty)
          } else {
            assert(vmin.isDefined)
            assertEquals(pat < 0, vpat.isEmpty)
            assertEquals(pat < 0, vpre.isEmpty)
            assertEquals(pat < 0, version.isEmpty)
            assertEquals(pat < 0, versionCore.isEmpty)
            assertEquals(pat < 0, coreVersion.isEmpty)
          }
        }
      }
    }
  }
}
