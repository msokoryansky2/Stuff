package TestMikeDataStruct

import MikeDataStruct.MikeMutableStack
import org.scalatest.FunSuite

class MikeMutableStackTest extends FunSuite {
  test("all basic ops work") {
    val s = new MikeMutableStack[Int]

    (0 to 3).foreach(n => {
      assert(s.pop === None)
      assert(s.isEmpty === true)
      assert(s.size === 0)
      assert(s.toString === "")

      s.push(1)
      assert(s.isEmpty === false)
      assert(s.size === 1)
      assert(s.toString === "1")

      s.push(2)
      assert(s.isEmpty === false)
      assert(s.size === 2)
      assert(s.toString === "2, 1")

      s.push(3)
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "3, 2, 1")

      s.push(4)
      assert(s.isEmpty === false)
      assert(s.size === 4)
      assert(s.toString === "4, 3, 2, 1")

      assert(s.pop === Some(4))
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "3, 2, 1")

      assert(s.pop === Some(3))
      assert(s.isEmpty === false)
      assert(s.size === 2)
      assert(s.toString === "2, 1")

      s.push(3)
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "3, 2, 1")

      s.push(4)
      assert(s.isEmpty === false)
      assert(s.size === 4)
      assert(s.toString === "4, 3, 2, 1")

      assert(s.pop === Some(4))
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "3, 2, 1")

      assert(s.pop === Some(3))
      assert(s.isEmpty === false)
      assert(s.size === 2)
      assert(s.toString === "2, 1")

      s.push(3)
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "3, 2, 1")

      s.push(4)
      assert(s.isEmpty === false)
      assert(s.size === 4)
      assert(s.toString === "4, 3, 2, 1")

      assert(s.pop === Some(4))
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "3, 2, 1")

      assert(s.pop === Some(3))
      assert(s.isEmpty === false)
      assert(s.size === 2)
      assert(s.toString === "2, 1")

      assert(s.pop === Some(2))
      assert(s.isEmpty === false)
      assert(s.size === 1)
      assert(s.toString === "1")

      assert(s.pop === Some(1))
      assert(s.isEmpty === true)
      assert(s.size === 0)
      assert(s.toString === "")

      assert(s.pop === None)
      assert(s.isEmpty === true)
      assert(s.size === 0)
      assert(s.toString === "")
    })
  }
}
