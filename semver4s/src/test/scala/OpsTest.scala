package semver4s

//import org.scalacheck.Prop.forAll

class OpsTest extends munit.ScalaCheckSuite {
  def v(src: String) = version(src).toOption.get

  test("inc examples") {
    assertEquals(v("1.2.4-beta.1"), Version.inc(v("1.2.4-beta.0")))
    assertEquals(v("1.2.5"), Version.inc(v("1.2.4")))
  }
}