import org.scalatest.FunSuite

class TestNumberField extends FunSuite {
  test("Non-rectangular or empty fields are invalid") {
    intercept[Exception] {
      NumberField(
        """
          |123  145
          |1236 63662 35
          |56   232
        """.stripMargin)
    }

    intercept[Exception] {
      NumberField(
        """
          |
        """.stripMargin)
    }
  }

  test("Rectangular field are cool") {
    val field = NumberField(
      """
        |123  0145
        |1236 63662
        |56   0232
      """.stripMargin)

    assert(field.height === 3)
    assert(field.width === 2)
    assert(field.elSize === 5)
    assert(field.el(0, 0) === 123)
    assert(field.el(1, 2) === 232)
  }

  val field = NumberField(
    """
      |001 002 003 004
      |005 006 007 009
      |010 011 012 013
      |001 002 003 004
    """.stripMargin)
  val path: List[(Int, Int)] = field.bestPath

  val field2 = NumberField(
    """
      |001 002 003 004
      |005 006 007 009
      |010 -11 12 -013
      |001 002 003 004
    """.stripMargin)
  val path2: List[(Int, Int)] = field2.bestPath

  test("bestPath returns path resulting in largest sum of visited numbers") {
    assert(path === List((0, 0), (0, 1), (0, 2), (1, 2), (2, 2), (3, 2), (3, 3)))
    assert(path2 === List((0, 0), (1, 0), (2, 0), (2, 1), (2, 2), (2, 3), (3, 3)))
  }

  test("evalPath evaluates path value") {
    assert(field.evalPath(path) === 56)
    assert(field2.evalPath(path2) === 34)
  }

  test("pathToString creates a visual representation of a path") {
    assert(field.pathToString(path) ===
      """
        |01 .. .. ..
        |05 .. .. ..
        |10 11 12 13
        |.. .. .. 04
      """.stripMargin.trim)

    assert(field2.pathToString(path2) ===
      """
        |001 002 003 ...
        |... ... 007 ...
        |... ... 012 ...
        |... ... 003 004
      """.stripMargin.trim)
  }
}
