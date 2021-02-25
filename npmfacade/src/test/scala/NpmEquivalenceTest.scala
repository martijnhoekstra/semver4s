package semver4s

import org.scalacheck.Prop.forAll

class NpmEquivalenceTest extends munit.ScalaCheckSuite {
  import semver4s.npm.NPMSemver
  import GenMatcher._
  import GenVersion._

  val maxSafeInt = 9007199254740991L


  property("Semver4s and NPM are consistent in what versions satisfy a range") {
    forAll(genRangeSet, genVersion)((rs, v) => {
      val isJsSafeVersion = v.major <= maxSafeInt && v.minor <= maxSafeInt && v.patch <= maxSafeInt
      val versionString = v.format
      val m = matcher(rs).toOption.get
      if (isJsSafeVersion){
        assertEquals(NPMSemver.satisfies(clue(versionString), clue(rs)), m.matches(v))
      }
    })
  }

  
}