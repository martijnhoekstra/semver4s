package semver4s

sealed trait Partial {
  def increment: Partial
  def version: Version
}

object Partial {
  case object Wild extends Partial {
    def increment = Wild
    def version   = Version(0, 0, 0)
  }

  case class Major(major: Long) extends Partial {
    def increment = Major(major + 1)
    def version   = Version(major, 0, 0)
  }

  case class Minor(major: Long, minor: Long) extends Partial {
    def increment = Minor(major, minor + 1)
    def version   = Version(major, minor, 0)
  }

  case class Patch(major: Long, minor: Long, patch: Long) extends Partial {
    def increment = Patch(major, minor, patch + 1)
    def version   = Version(major, minor, patch)
  }

  case class Pre(major: Long, minor: Long, patch: Long, pre: SemVer.PreReleaseSuffix)
      extends Partial {
    def increment = ???
    def version   = Version(major, minor, patch, Some(pre), None)
  }
}
