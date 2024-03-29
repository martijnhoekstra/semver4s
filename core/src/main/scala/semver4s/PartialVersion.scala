package semver4s

sealed trait Partial {
  def increment: Partial
  def version: Version
  override def toString() = Partial.print(this)
}

object Partial {
  import PartialFunction.condOpt

  def unsafe(major: Long)                           = new Major(major) {}
  def unsafe(major: Long, minor: Long)              = new Minor(major, minor) {}
  def unsafe(major: Long, minor: Long, patch: Long) = new Patch(major, minor, patch) {}
  def unsafe(major: Long, minor: Long, patch: Long, pre: SemVer.PreReleaseSuffix) =
    new Pre(major, minor, patch, pre) {}

  def apply(major: Long) = condOpt(major) {
    case m if m >= 0 => new Major(major) {}
  }
  def apply(major: Long, minor: Long) = condOpt(major -> minor) {
    case (maj, min) if maj >= 0 && min >= 0 => new Minor(major, minor) {}
  }
  def apply(major: Long, minor: Long, patch: Long) = condOpt(major -> minor -> patch) {
    case ((maj, min), pat) if maj >= 0 && min >= 0 && pat >= 0 => new Patch(major, minor, patch) {}
  }
  def apply(major: Long, minor: Long, patch: Long, pre: SemVer.PreReleaseSuffix) =
    condOpt(major -> minor -> patch -> pre) {
      case (((maj, min), pat), pre) if maj >= 0 && min >= 0 && pat >= 0 =>
        new Pre(major, minor, patch, pre) {}
    }.filterNot(_ => pre.isEmpty)

  case object Wild extends Partial {
    def increment = Wild
    def version   = Version.unsafe(0, 0, 0)
  }

  sealed abstract case class Major(major: Long) extends Partial {
    def increment =
      apply(major + 1).getOrElse(throw new ArithmeticException(s"overflow on major version"))
    def version = Version.unsafe(major, 0, 0)
  }
  sealed abstract case class Minor(major: Long, minor: Long) extends Partial {
    def incrementMajor =
      apply(major + 1).getOrElse(throw new ArithmeticException(s"overflow on major version"))
    def increment = apply(major, minor + 1)
      .orElse(apply(major + 1, 0))
      .getOrElse(throw new ArithmeticException(s"overflow on major version"))
    def version = Version.unsafe(major, minor, 0)
  }

  sealed abstract case class Patch(major: Long, minor: Long, patch: Long) extends Partial {
    def incrementMajor =
      apply(major + 1).getOrElse(throw new ArithmeticException(s"overflow on major version"))
    def incrementMinor = apply(major, minor + 1)
      .orElse(apply(major + 1, 0))
      .getOrElse(throw new ArithmeticException(s"overflow on major version"))
    def increment = apply(major, minor, patch + 1)
      .orElse(apply(major, minor + 1, 0))
      .orElse(apply(major + 1, 0, 0))
      .getOrElse(throw new ArithmeticException(s"overflow on major version"))
    def version = Version.unsafe(major, minor, patch)
  }

  sealed abstract case class Pre(
      major: Long,
      minor: Long,
      patch: Long,
      pre: SemVer.PreReleaseSuffix
  ) extends Partial {
    def incrementMajor =
      apply(major + 1).getOrElse(throw new ArithmeticException(s"overflow on major version"))
    def incrementMinor = apply(major, minor + 1)
      .orElse(apply(major + 1, 0))
      .getOrElse(throw new ArithmeticException(s"overflow on major version"))
    def incrementPatch = apply(major, minor, patch + 1)
      .orElse(apply(major, minor + 1, 0))
      .orElse(apply(major + 1, 0, 0))
      .getOrElse(throw new ArithmeticException(s"overflow on major version"))
    def increment = pre.last match {
      case Left(str) =>
        unsafe(major, minor, patch, pre.init :+ Left(str + "-"))
      case Right(l) =>
        unsafe(major, minor, patch, pre.init :+ Right(l + 1))
    }
    def version = Version.unsafe(major, minor, patch, pre)
  }

  def print(p: Partial): String = p match {
    case Wild                       => "*"
    case Major(major)               => s"$major.*"
    case Minor(major, minor)        => s"$major.$minor.*"
    case Patch(major, minor, patch) => s"$major.$minor.$patch"
    case Pre(major, minor, patch, pre) => {
      val preString = pre.toList.map(_.fold(identity, _.toString)).mkString(".")
      s"$major.$minor.$patch-$preString"
    }
  }
}
