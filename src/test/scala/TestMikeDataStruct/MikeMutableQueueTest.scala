package TestMikeDataStruct

import MikeDataStruct.MikeMutableQueue
import org.scalatest.FunSuite

class MikeMutableQueueTest extends FunSuite {
  test("all basic ops work") {
    val s = new MikeMutableQueue[Int]

    (0 to 3).foreach(n => {
      assert(s.dequeue === None)
      assert(s.isEmpty === true)
      assert(s.size === 0)
      assert(s.toString === "")

      s.enqueue(1)
      assert(s.isEmpty === false)
      assert(s.size === 1)
      assert(s.toString === "1")

      s.enqueue(2)
      assert(s.isEmpty === false)
      assert(s.size === 2)
      assert(s.toString === "2, 1")

      s.enqueue(3)
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "3, 2, 1")

      s.enqueue(4)
      assert(s.isEmpty === false)
      assert(s.size === 4)
      assert(s.toString === "4, 3, 2, 1")

      assert(s.dequeue === Some(1))
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "4, 3, 2")

      assert(s.dequeue === Some(2))
      assert(s.isEmpty === false)
      assert(s.size === 2)
      assert(s.toString === "4, 3")

      s.enqueue(3)
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "3, 4, 3")

      s.enqueue(4)
      assert(s.isEmpty === false)
      assert(s.size === 4)
      assert(s.toString === "4, 3, 4, 3")

      assert(s.dequeue === Some(3))
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "4, 3, 4")

      assert(s.dequeue === Some(4))
      assert(s.isEmpty === false)
      assert(s.size === 2)
      assert(s.toString === "4, 3")

      s.enqueue(3)
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "3, 4, 3")

      s.enqueue(4)
      assert(s.isEmpty === false)
      assert(s.size === 4)
      assert(s.toString === "4, 3, 4, 3")

      assert(s.dequeue === Some(3))
      assert(s.isEmpty === false)
      assert(s.size === 3)
      assert(s.toString === "4, 3, 4")

      assert(s.dequeue === Some(4))
      assert(s.isEmpty === false)
      assert(s.size === 2)
      assert(s.toString === "4, 3")

      assert(s.dequeue === Some(3))
      assert(s.isEmpty === false)
      assert(s.size === 1)
      assert(s.toString === "4")

      assert(s.dequeue === Some(4))
      assert(s.isEmpty === true)
      assert(s.size === 0)
      assert(s.toString === "")

      assert(s.dequeue === None)
      assert(s.isEmpty === true)
      assert(s.size === 0)
      assert(s.toString === "")
    })
  }
}
