package semver4s

import scala.reflect.macros.blackbox.Context

object Literal {

  def version(s: String): Version = macro versionImpl
  def matcher(s: String): Matcher = macro matcherImpl

  def versionImpl(c: Context)(s: c.Expr[String]) = versionFromTree(c)(s.tree)
  def matcherImpl(c: Context)(s: c.Expr[String]) = matcherFromTree(c)(s.tree)

  def versionFromTree(c: Context)(t: c.Tree) = {
    import c.{universe => u}
    import u._
    t match {
      case u.Literal(u.Constant(s: String)) =>
        def r = new parsing.Reporter(s)
        semver4s
          .version(s)
          .fold(
            e => c.abort(c.enclosingPosition, r.report(e).toList.mkString),
            _ => q"_root_.semver4s.parsing.SemverParser.semver.parseAll($s).toOption.get"
          )
      case _ =>
        c.abort(
          c.enclosingPosition,
          s"This method uses a macro to verify that a String literal is a valid version. semver4s.version if you have a dynamic String that you want to parse."
        )
    }
  }

  def matcherFromTree(c: Context)(t: c.Tree) = {
    import c.{universe => u}
    import u._
    t match {
      case u.Literal(u.Constant(s: String)) =>
        def r = new parsing.Reporter(s)
        semver4s
          .matcher(s)
          .fold(
            e => c.abort(c.enclosingPosition, r.report(e).toList.mkString),
            _ => q"_root_.semver4s.parsing.MatcherParser.rangeSet.parseAll($s).toOption.get"
          )
      case _ =>
        c.abort(
          c.enclosingPosition,
          s"This method uses a macro to verify that a String literal is a valid matcher. use semver4s.matcher if you have a dynamic String that you want to parse."
        )
    }
  }

  implicit class Extensions(val ctx: StringContext) extends AnyVal {
    def m(args: Any*): Matcher = macro mImpl
    def v(args: Any*): Version = macro vImpl
  }

  def mImpl(c: Context)(args: c.Expr[Any]*): c.universe.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(_, List(Apply(_, List(single)))) =>
        matcherFromTree(c)(single)
      case _ =>
        c.abort(
          c.enclosingPosition,
          s"matcher interpolators can't have any values interpolated, but $args found"
        )
    }
  }

  def vImpl(c: Context)(args: c.Expr[Any]*): c.universe.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(_, List(Apply(_, List(single)))) =>
        versionFromTree(c)(single)
      case _ =>
        c.abort(
          c.enclosingPosition,
          s"version interpolators can't have any values interpolated, but $args found"
        )
    }
  }

}
