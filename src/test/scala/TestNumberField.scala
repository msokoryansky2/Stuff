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
    assert(field.el(0,0) === 123)
    assert(field.el(2, 1) === 232)

    println(field)
  }
}
