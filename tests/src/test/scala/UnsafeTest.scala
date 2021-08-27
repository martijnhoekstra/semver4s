package semver4s

class UnsafeTest extends munit.ScalaCheckSuite {

  import unsafe._

  test("satisfies") {
    assert("1.2.3".satisfies(">=1 <2.x"))
  }

  test("matches") {
    assert(">=1 <2.x".matchesVersion("1.2.3"))
  }

  test("invalid") {
    val i1 = intercept[IllegalArgumentException]("something".satisfies(">=1 <2"))
    assert(i1.getMessage().startsWith("invalid version"))
    val i2 = intercept[IllegalArgumentException](">=1 <2".matchesVersion("something"))
    assert(i2.getMessage().startsWith("invalid version"))
    val i3 = intercept[IllegalArgumentException]("1.2.3".satisfies("something"))
    assert(i3.getMessage().startsWith("invalid matcher"))
    val i4 = intercept[IllegalArgumentException]("something".matchesVersion("1.2.3"))
    assert(i4.getMessage().startsWith("invalid matcher"))
  }
}
