/*
 * Copyright (c) 2020 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package semver4s

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

class SemverParserTest extends munit.ScalaCheckSuite {

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
        assertEquals(SemverParsers.preRelease.string.parseAll(pre), Right(pre))
      }
    }
  }

  property(("semver parsers metadata")) {
    forAll(genMetadata) { (meta: String) =>
      {
        assertEquals(SemverParsers.build.string.parseAll(meta), Right(meta))
      }
    }
  }

  property("semver parses SemVer") {
    forAll(genSemVer) { (sv: String) =>
      {
        assertEquals(SemverParsers.semver.string.parseAll(sv), Right(sv))
      }
    }
  }
}
