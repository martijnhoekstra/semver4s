package semver4s

import cats.kernel.Order
import cats.kernel.Comparison._
import cats.syntax.all._
import java.text.Collator

object VersionOrder {
  import SemVer._

  /** ASCII-betical sort order for strings
    * 
    * A lexical ordering where individual characters are sorted based on their
    * codepoint value
    */
  val ASCIIbetical: Order[String] = Order.from((l, r) => l.compareTo(r))

  /** Lexical order based on the given collator
    */
  def collationOrder(collator: Collator): Order[String] = Order.from(collator.compare)

  /** Order of identifiers as defined by SemVer
    * 
    * Sorts numerical identifiers before alphanumeric identifiers.
    * 
    * Numerical identifiers are sorted by their numeric value, alphanumeric
    * identifiers are sorted ascii-betically.
    */
  val identifierPrecedence: Order[Identifier] = Order.from {
    case (Left(str1), Left(str2)) => ASCIIbetical.compare(str1, str2)
    case (Right(l1), Right(l2))   => Order.compare(l1, l2)
    case (Left(_: String), Right(_: Long)) =>
      GreaterThan.toInt //numerical sorts before alphanumerical
    case (Right(_), Left(_)) => LessThan.toInt
  }

  //given identical prefixes, the shorter one comes *after* the longer one
  //this subsumes the case that no pre-release comes its pre-release
  def preReleasePrecedence(l1: List[Identifier], l2: List[Identifier]): Int = (l1, l2) match {
    case (Nil, Nil) => EqualTo.toInt
    case (Nil, _)   => GreaterThan.toInt
    case (_, Nil)   => LessThan.toInt
    case (i1 :: lt, i2 :: rt) =>
      identifierPrecedence.compare(i1, i2) match {
        case 0 => preReleasePrecedence(lt, rt)
        case i => i
      }
  }

  val preReleaseOrder: Order[Option[PreReleaseSuffix]] =
    Order.from(preReleasePrecedence).contramap(_.map(_.toList).getOrElse(Nil))

}
