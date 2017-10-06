object Quine extends App {
  val s =
    """
      |Object Quine extends App {
      |  val s =
      |    ""%2$s%1$s%2$s""
      |  // This is a totally unnecessary comment
      |  println(s.stripMargin.format(s, "\""))
      |}
    """
  // This is a totally unnecessary comment
  println(s.stripMargin.format(s, "\""))
}
