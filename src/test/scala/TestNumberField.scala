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

  test("bestPath returns path resulting in largest sum of visited numbers") {
    val field = NumberField(
      """
        |001 002 003 004
        |005 006 007 009
        |010 011 012 013
        |001 002 003 004
      """.stripMargin)

    assert(field.bestPath === List((0, 0), (0, 1), (0, 2), (1, 2), (2, 2), (3, 2), (3, 3)))
    assert(field.evalPath(field.bestPath) === 56)
  }
}
