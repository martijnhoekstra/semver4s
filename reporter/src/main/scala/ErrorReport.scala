package catsparse.reporting

import cats.data.NonEmptyList

/** An error report, consisting of the context around which the error occurred, A pointer to the
  * error positon and a list of formatted messages describing the failed expectations at that point
  *
  * @param context
  * @param caretPosition
  * @param messages
  */
case class ErrorReport(context: String, caretPosition: Int, messages: NonEmptyList[String]) {
  def caretLine = " " * caretPosition + "^"
  def messagesAsBlock(separator: String = "\n"): String = messages match {
    case NonEmptyList(head, Nil) => head
    case _ => messages.toList.mkString(s"any of the following:$separator", s"Or $separator", "")
  }
}
