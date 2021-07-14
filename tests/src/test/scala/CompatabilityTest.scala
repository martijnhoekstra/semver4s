package semver4s

import org.scalacheck.Prop.forAll
import semver4s.gen.GenVersion
//import Shrinkers._

class CompatibilityTest extends munit.ScalaCheckSuite {

  property("compatible versions have the same major version") {
    forAll(GenVersion.genVersion, GenVersion.genVersion) { (v1, v2) =>
      if (v1.isCompatibleWith(v2)) {
        assertEquals(v1.major, v2.major)
      }
    }
  }

  property(
    "compatible versions have either the same pre-release tag, or neither has a pre-release tag"
  ) {
    forAll(GenVersion.genVersion, GenVersion.genVersion) { (v1, v2) =>
      if (v1.isCompatibleWith(v2)) {
        assertEquals(v1.pre, v2.pre)
      }
    }
  }

  property("versions compatible per SemVer are also compatible per early SemVer") {
    forAll(GenVersion.genVersion, GenVersion.genVersion) { (v1, v2) =>
      if (v1.isCompatibleWith(v2)) {
        assert(v1.isEarlyCompatibleWith(v2))
      }
    }
  }

  property(
    "versions compatible per early SemVer with a version >= 1 are also compatible per SemVer"
  ) {
    forAll(GenVersion.genVersion, GenVersion.genVersion) { (v1, v2) =>
      if (v1.isEarlyCompatibleWith(v2) && v1.major >= 1) {
        assert(v1.isCompatibleWith(v2))
      }
    }
  }

  property("No versions < 1 are required to be compatible per SemVer, unless they are identical") {
    forAll(GenVersion.genVersion, GenVersion.genVersion) { (v1, v2) =>
      {
        def early(v: Version) = v.pre match {
          case Nil => Version.unsafe(0, v.minor, v.patch)
          case p   => Version.unsafe(0, v.minor, v.patch, p)
        }
        val ev1 = early(v1)
        val ev2 = early(v2)
        if (ev1.isCompatibleWith(ev2)) assert(v1 == v2)
      }
    }
  }
}
