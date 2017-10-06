object Quine extends App {
  val s =
    """
      |Object Quine extends App {
      |  val s =
      |    ""%2$s%1$s%2$s""
      |  println(s.stripMargin.format(s, "\""))
      |}
    """
  println(s.stripMargin.format(s, "\""))
}
