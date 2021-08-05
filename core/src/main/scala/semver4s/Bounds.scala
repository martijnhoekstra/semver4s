package semver4s

sealed trait Bound
object Bound {
  case object Unbounded extends Bound
  sealed trait Bounded extends Bound {
    def by: Version
  }
  case class Inclusive(by: Version) extends Bounded
  case class Exclusive(by: Version) extends Bounded
}
