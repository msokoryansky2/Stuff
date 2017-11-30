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

object Quine2 extends App {
  val s =
    """
      |object Quine2 extends App {
      |  val s =
      |    ""%2$s%1$s%2$s""
      |  println(s.stripMargin.format(s, "\""))
      |}
    """
    println(s.stripMargin.format(s, "\""))
}

object Quine3 extends App { val s = "\\"; val q = "\""; val p = "object Quine3 extends App { val s = %1$s%3$s%3$s%1$s; val q = %1$s%3$s%1$s%1$s; val p = %1$s%2$s%1$s; println(p.format(q, p, s)) }"; println(p.format(q, p, s)) }



object Quine4a extends App { val p = "object Quine4a extends App { val p = %1$s; println(p.format(p))}"; println(p.format(p))}

object Quine4b extends App { val p = "object Quine4b extends App { val p = \"%1$s\"; println(p.format(p))}"; println(p.format(p))}

object Quine4c extends App { val q = "\""; val p = "object Quine4c extends App { val q = \"%2$s\"; val p = %2$s%1$s%2$s; println(p.format(p, q))}"; println(p.format(p, q))}

object Quine4d extends App { val s = "\\"; val q = "\""; val p = "object Quine4d extends App { val s = %2$s%3$s%3$s%2$s; val q = %2$s%3$s%2$s%2$s; val p = %2$s%1$s%2$s; println(p.format(p, q, s))}"; println(p.format(p, q, s))}
