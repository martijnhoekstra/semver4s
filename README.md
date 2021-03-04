[![javadoc](https://javadoc.io/badge2/com.heroestools/semver4s_2.13/javadoc.svg)](https://javadoc.io/doc/com.heroestools/semver4s_2.13)

## Semver4s

Parse SemVer, NPM-style SemVer ranges, and check whether some version matches
some range.

## Features

Parsers for semver:

```scala
import semver4s._

for {
  version <- parseVersion("1.2.3")
  matcher <- parseMatcher("~1.2")
} yield matcher.matches(version)
```

Support for literal versions and matchers with the `v` and `m` interpolator

```scala
import semver4s.Literal._

m"~1.2".matches(v"1.2.3")
```

Supports all npm version ranges as matchers
