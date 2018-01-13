import org.scalatest.FunSuite

class MarsLanderTest extends FunSuite {
  test("Length of Mars Lander paths to all POIs, coming back to landing after each") {
    intercept[Exception] {
      MarsLander("""
                  |
                 """.stripMargin, (0,0))
    }
    intercept[Exception] {
      MarsLander("""
                   |++
                   |+
                 """.stripMargin, (0,0))
    }
    intercept[Exception] {
      MarsLander("""
                   |+!+
                   |+++
                 """.stripMargin, (4,3))
    }
    assert(MarsLander(
      """
        |+++!++
        |+!++++
        |++++++
        |+++!!+
      """.stripMargin, (0,0)).pathLengthToAllPois === (3 + 2 + 6 + 7) * 2)
    assert(MarsLander(
      """
        |+++!++
        |+!++++
        |++++++
        |+++!!+
      """.stripMargin, (2,3)).pathLengthToAllPois === (2 + 3 + 1 + 2) * 2)
    assert(MarsLander(
      """
        |+++!++
        |+!++++
        |++++++
        |+++!!+
      """.stripMargin, (0,3)).pathLengthToAllPois === (0 + 3 + 3 + 4) * 2)
  }
}
