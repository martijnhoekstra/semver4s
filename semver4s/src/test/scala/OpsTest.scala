package semver4s

class OpsTest extends munit.ScalaCheckSuite {
  import Literal._
  test("inc examples") {
    assertEquals(v"1.2.4-beta.1", v"1.2.4-beta.0".increment)
    assertEquals(v"1.2.5", v"1.2.4".increment)
  }
}
