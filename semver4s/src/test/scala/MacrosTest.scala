package semver4s

class MacrosTest extends munit.ScalaCheckSuite {
  import Literal._
  test("version interpolator") {
    assertEquals(Version.unsafe(1, 2, 3), v"1.2.3")
  }

  test("matcher interpolator") {
    val matcher = m">1 || ~0.3.1"
    assert(matcher.matches(v"2.0.0"))
    assert(matcher.matches(v"0.3.2"))

  }
}
