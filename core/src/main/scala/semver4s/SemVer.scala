package semver4s

object SemVer {
  type Identifier       = Either[String, Long]
  type PreReleaseSuffix = List[Identifier]
}
