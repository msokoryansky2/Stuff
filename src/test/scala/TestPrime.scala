import org.scalatest.FunSuite

class TestPrime extends FunSuite {
  test("primes should return 2, 3, 5, 7, 11, 13, ...") {
    assert(Prime.primes.take(10).toList === List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
  }
}
