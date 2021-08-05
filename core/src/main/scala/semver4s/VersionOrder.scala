package semver4s

import java.text.Collator

object VersionOrder {
  private final implicit class OrderingFrom(val o: Ordering.type) extends AnyVal {
    def from[A](f: (A, A) => Int): Ordering[A] = Ordering.comparatorToOrdering {
      new java.util.Comparator[A]() {
        override def compare(a1: A, a2: A) = f(a1, a2)
      }
    }
  }
  import SemVer._

  /** ASCIIbetical sort Ordering for strings
    *
    * A lexical Ordering where individual characters are sorted based on their codepoint value
    */
  val ASCIIbetical: Ordering[String] = Ordering.from((l, r) => l.compareTo(r))

  /** Lexical Ordering based on the given collator
    */
  def collationOrdering(collator: Collator): Ordering[String] = Ordering.from(collator.compare)

  /** Ordering of identifiers as defined by SemVer
    *
    * Sorts numerical identifiers before alphanumeric identifiers.
    *
    * Numerical identifiers are sorted by their numeric value, alphanumeric identifiers are sorted
    * ascii-betically.
    */
  val identifierPrecedence: Ordering[Identifier] = Ordering.from {
    case (Left(str1), Left(str2)) => ASCIIbetical.compare(str1, str2)
    case (Right(l1), Right(l2))   => Ordering.Long.compare(l1, l2)
    case (Left(_: String), Right(_: Long)) =>
      1 //numerical sorts before alphanumerical
    case (Right(_), Left(_)) => -1
  }

  //given identical prefixes, the shorter one comes *after* the longer one
  //this subsumes the case that no pre-release comes its pre-release
  def preReleasePrecedence(l1: List[Identifier], l2: List[Identifier]): Int = (l1, l2) match {
    case (Nil, Nil) => 0
    case (Nil, _)   => 1
    case (_, Nil)   => -1
    case (i1 :: lt, i2 :: rt) =>
      identifierPrecedence.compare(i1, i2) match {
        case 0 => preReleasePrecedence(lt, rt)
        case i => i
      }
  }

  val preReleaseOrder: Ordering[PreReleaseSuffix] = Ordering.from(preReleasePrecedence)

}
