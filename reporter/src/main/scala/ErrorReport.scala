package catsparse.reporting

import cats.data.NonEmptyList

/** An error report, consisting of the context around which the error occurred, A pointer to the
  * error positon and a list of formatted expectations describing what was expected in that location
  *
  * @param context
  *   the text around the error
  * @param caretPosition
  *   the position of the caret pointing at the error in the context
  * @param expectations
  *   a list of desciptions of the expectations that the error failed
  */
case class ErrorReport(context: String, caretPosition: Int, expectations: NonEmptyList[String]) {
  def caretLine = " " * caretPosition + "^"
  def messagesAsBlock(separator: String = "\n"): String = expectations match {
    case NonEmptyList(head, Nil) => s"Expected $head"
    case _ =>
      expectations.toList.mkString(
        s"Expected any of the following:$separator",
        s" Or$separator",
        ""
      )
  }
}
