package TestMikeDataStruct

import MikeDataStruct.MikeMutableLinkedList
import org.scalatest.FunSuite

class MikeMutableLinkedListTest extends FunSuite {
  test("Can getFirst, getLast, peek, push, pop, peekLast, pushLast, and popLast. Can keep track length along the way and check if value is present") {
    val l = new MikeMutableLinkedList[Int]

    assert(l.length === 0)
    assert(l.peek === None)
    assert(l.peekLast === None)
    assert(l.contains(0) === false)
    assert(l.contains(1) === false)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    l.pop
    assert(l.length === 0)
    assert(l.peek === None)
    assert(l.peekLast === None)
    assert(l.contains(0) === false)
    assert(l.contains(1) === false)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    l.popLast
    assert(l.length === 0)
    assert(l.peek === None)
    assert(l.peekLast === None)
    assert(l.contains(0) === false)
    assert(l.contains(1) === false)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    l.push(1)
    assert(l.peek === Some(1))
    assert(l.peekLast === Some(1))
    assert(l.length === 1)
    assert(l.toString === "1")
    assert(l.contains(0) === false)
    assert(l.contains(1) === true)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    l.push(2)
    assert(l.peek === Some(2))
    assert(l.peekLast === Some(1))
    assert(l.length === 2)
    assert(l.toString === "2 -> 1")
    assert(l.contains(0) === false)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    l.push(3)
    assert(l.peek === Some(3))
    assert(l.peekLast === Some(1))
    assert(l.length === 3)
    assert(l.toString === "3 -> 2 -> 1")
    assert(l.contains(0) === false)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    l.push(4)
    assert(l.peek === Some(4))
    assert(l.peekLast === Some(1))
    assert(l.length === 4)
    assert(l.toString === "4 -> 3 -> 2 -> 1")
    assert(l.contains(0) === false)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    l.pushLast(0)
    assert(l.peek === Some(4))
    assert(l.peekLast === Some(0))
    assert(l.length === 5)
    assert(l.toString === "4 -> 3 -> 2 -> 1 -> 0")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    l.pushLast(-1)
    assert(l.peek === Some(4))
    assert(l.peekLast === Some(-1))
    assert(l.length === 6)
    assert(l.toString === "4 -> 3 -> 2 -> 1 -> 0 -> -1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.pop === Some(4))
    assert(l.peek === Some(3))
    assert(l.peekLast === Some(-1))
    assert(l.length === 5)
    assert(l.toString === "3 -> 2 -> 1 -> 0 -> -1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.pop === Some(3))
    assert(l.peek === Some(2))
    assert(l.peekLast === Some(-1))
    assert(l.length === 4)
    assert(l.toString === "2 -> 1 -> 0 -> -1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.popLast === Some(-1))
    assert(l.peek === Some(2))
    assert(l.peekLast === Some(0))
    assert(l.length === 3)
    assert(l.toString === "2 -> 1 -> 0")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.popLast === Some(0))
    assert(l.peek === Some(2))
    assert(l.peekLast === Some(1))
    assert(l.length === 2)
    assert(l.toString === "2 -> 1")
    assert(l.contains(0) === false)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    l.pushLast(0)
    assert(l.peek === Some(2))
    assert(l.peekLast === Some(0))
    assert(l.length === 3)
    assert(l.toString === "2 -> 1 -> 0")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.popLast === Some(0))
    assert(l.peek === Some(2))
    assert(l.peekLast === Some(1))
    assert(l.length === 2)
    assert(l.toString === "2 -> 1")
    assert(l.contains(0) === false)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.popLast === Some(1))
    assert(l.peek === Some(2))
    assert(l.peekLast === Some(2))
    assert(l.length === 1)
    assert(l.toString === "2")
    assert(l.contains(0) === false)
    assert(l.contains(1) === false)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.popLast === Some(2))
    assert(l.peek === None)
    assert(l.peekLast === None)
    assert(l.length === 0)
    assert(l.toString === "")
    assert(l.contains(0) === false)
    assert(l.contains(1) === false)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    l.push(1)
    assert(l.peek === Some(1))
    assert(l.peekLast === Some(1))
    assert(l.length === 1)
    assert(l.toString === "1")
    assert(l.contains(0) === false)
    assert(l.contains(1) === true)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    assert(l.pop === Some(1))
    assert(l.peek === None)
    assert(l.peekLast === None)
    assert(l.length === 0)
    assert(l.toString === "")
    assert(l.contains(0) === false)
    assert(l.contains(1) === false)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    l.push(1)
    assert(l.peek === Some(1))
    assert(l.peekLast === Some(1))
    assert(l.length === 1)
    assert(l.toString === "1")
    assert(l.contains(0) === false)
    assert(l.contains(1) === true)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    assert(l.popLast === Some(1))
    assert(l.peek === None)
    assert(l.peekLast === None)
    assert(l.length === 0)
    assert(l.toString === "")
    assert(l.contains(0) === false)
    assert(l.contains(1) === false)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)
  }

  test("insertBefore and remove allow insertion and removal from any place in the list") {
    val l = new MikeMutableLinkedList[Int]

    l.push(1)
    assert(l.peek === Some(1))
    assert(l.peekLast === Some(1))
    assert(l.length === 1)
    assert(l.toString === "1")
    assert(l.contains(0) === false)
    assert(l.contains(1) === true)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    assert(l.insertBefore(2, 3) === None)
    assert(l.peek === Some(1))
    assert(l.peekLast === Some(1))
    assert(l.length === 1)
    assert(l.toString === "1")
    assert(l.contains(0) === false)
    assert(l.contains(1) === true)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    assert(l.insertBefore(0, 1) === Some(0))
    assert(l.peek === Some(0))
    assert(l.peekLast === Some(1))
    assert(l.length === 2)
    assert(l.toString === "0 -> 1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === false)
    assert(l.contains(1000) === false)

    assert(l.insertBefore(2, 1) === Some(2))
    assert(l.peek === Some(0))
    assert(l.peekLast === Some(1))
    assert(l.length === 3)
    assert(l.toString === "0 -> 2 -> 1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.insertBefore(2, 1000) === None)
    assert(l.peek === Some(0))
    assert(l.peekLast === Some(1))
    assert(l.length === 3)
    assert(l.toString === "0 -> 2 -> 1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.insertBefore(3, 1) === Some(3))
    assert(l.peek === Some(0))
    assert(l.peekLast === Some(1))
    assert(l.length === 4)
    assert(l.toString === "0 -> 2 -> 3 -> 1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.insertBefore(-1, 0) === Some(-1))
    assert(l.peek === Some(-1))
    assert(l.peekLast === Some(1))
    assert(l.length === 5)
    assert(l.toString === "-1 -> 0 -> 2 -> 3 -> 1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.insertBefore(-2, 2) === Some(-2))
    assert(l.peek === Some(-1))
    assert(l.peekLast === Some(1))
    assert(l.length === 6)
    assert(l.toString === "-1 -> 0 -> -2 -> 2 -> 3 -> 1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.remove(10000) === None)
    assert(l.peek === Some(-1))
    assert(l.peekLast === Some(1))
    assert(l.length === 6)
    assert(l.toString === "-1 -> 0 -> -2 -> 2 -> 3 -> 1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.remove(-2) === Some(-2))
    assert(l.peek === Some(-1))
    assert(l.peekLast === Some(1))
    assert(l.length === 5)
    assert(l.toString === "-1 -> 0 -> 2 -> 3 -> 1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.remove(-1) === Some(-1))
    assert(l.peek === Some(0))
    assert(l.peekLast === Some(1))
    assert(l.length === 4)
    assert(l.toString === "0 -> 2 -> 3 -> 1")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.remove(1) === Some(1))
    assert(l.peek === Some(0))
    assert(l.peekLast === Some(3))
    assert(l.length === 3)
    assert(l.toString === "0 -> 2 -> 3")
    assert(l.contains(0) === true)
    assert(l.contains(1) === false)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)

    assert(l.insertBefore(1, 2) === Some(1))
    assert(l.peek === Some(0))
    assert(l.peekLast === Some(3))
    assert(l.length === 4)
    assert(l.toString === "0 -> 1 -> 2 -> 3")
    assert(l.contains(0) === true)
    assert(l.contains(1) === true)
    assert(l.contains(2) === true)
    assert(l.contains(1000) === false)
  }

  test("reverse reverses the list") {
    val l = new MikeMutableLinkedList[Int]
    assert(l.toString === "")

    l.reverse()
    assert(l.toString === "")

    l.push(1)
    assert(l.toString === "1")
    l.reverse()
    assert(l.toString === "1")

    l.push(2)
    assert(l.toString === "2 -> 1")
    l.reverse()
    assert(l.toString === "1 -> 2")

    l.push(3)
    assert(l.toString === "3 -> 1 -> 2")
    l.reverse()
    assert(l.toString === "2 -> 1 -> 3")

    l.insertBefore(4, 1)
    assert(l.toString === "2 -> 4 -> 1 -> 3")
    l.reverse()
    assert(l.toString === "3 -> 1 -> 4 -> 2")

    assert(l.pop === Some(3))
    assert(l.toString === "1 -> 4 -> 2")
    l.reverse()
    assert(l.toString === "2 -> 4 -> 1")

    assert(l.pop === Some(2))
    assert(l.toString === "4 -> 1")
    l.reverse()
    assert(l.toString === "1 -> 4")

    assert(l.pop === Some(1))
    assert(l.toString === "4")
    l.reverse()
    assert(l.toString === "4")

    assert(l.pop === Some(4))
    assert(l.toString === "")
    l.reverse()
    assert(l.toString === "")
  }
}
