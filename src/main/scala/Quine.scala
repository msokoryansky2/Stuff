/**
  * Wiki example of a Scala Quine
  */
object Quine extends App {
  val s =
    """
      |object Quine extends App {
      |  val s =
      |    ""%2$s%1$s%2$s""
      |  // This is a totally unnecessary comment
      |  println(s.stripMargin.format(s, "\""))
      |}
    """
  // This is a totally unnecessary comment
  println(s.stripMargin.format(s, "\""))
}


/**
  * Quine4a through Quine4d are step-by-step build of my own Quine (4d being the last step)
  */
object Quine4a extends App { val p = "object Quine4a extends App { val p = %1$s; println(p.format(p))}"; println(p.format(p))}
object Quine4b extends App { val p = "object Quine4b extends App { val p = \"%1$s\"; println(p.format(p))}"; println(p.format(p))}
object Quine4c extends App { val q = "\""; val p = "object Quine4c extends App { val q = \"%2$s\"; val p = %2$s%1$s%2$s; println(p.format(p, q))}"; println(p.format(p, q))}
object Quine4d extends App { val s = "\\"; val q = "\""; val p = "object Quine4d extends App { val s = %2$s%3$s%3$s%2$s; val q = %2$s%3$s%2$s%2$s; val p = %2$s%1$s%2$s; println(p.format(p, q, s))}"; println(p.format(p, q, s))}


/**
  * My own multiline Quine without using multiline strings
  */
object Quine5 extends App {
  val q = "\""
  val e = "\\"
  val n = "\n"
  val p = "object Quine5 extends App {%3$s  val q = %1$s%2$s%1$s%1$s%3$s  val e = %1$s%2$s%2$s%1$s%3$s  val n = %1$s%2$sn%1$s%3$s  val p = %1$s%4$s%1$s%3$s  println(p.format(q, e, n, p))%3$s}"
  println(p.format(q, e, n, p))
}