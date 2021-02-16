package semver4s

sealed trait PreReleaseBehaviour

object PreReleaseBehaviour {
  case object Loose extends PreReleaseBehaviour

  /** Never match a pre-release version
    */
  case object Never extends PreReleaseBehaviour

  /** Match a pre-release version if the matchers version also
    * has a pre-release version
    */
  case object Strict extends PreReleaseBehaviour
}
