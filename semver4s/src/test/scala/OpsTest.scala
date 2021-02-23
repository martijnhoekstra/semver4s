package semver4s

class OpsTest extends munit.ScalaCheckSuite {
  import Literal._
  test("inc examples") {
    assertEquals(v"1.2.4-beta.1", Version.inc(v"1.2.4-beta.0"))
    assertEquals(v"1.2.5", Version.inc(v"1.2.4"))
  }
}
