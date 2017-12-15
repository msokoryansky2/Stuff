package TestMikeDataStruct

import MikeDataStruct.MikeDoublyLinkedList
import org.scalatest.FunSuite

class TestMikeDoublyLinkedList extends FunSuite {

  assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).size === 3)

  test("isLast and isFirst test if node is start or end of the list") {
    assert(new MikeDoublyLinkedList[Int](5, None, None).isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).isLast === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.prepend(7).isLast === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.prepend(7).remove.get.isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).isLast === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).remove.get.isLast === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).remove.get.remove.get.isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).isFirst === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).isFirst === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.isFirst === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.prepend(7).isFirst === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.prepend(7).remove.get.isFirst === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).isFirst === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).remove.get.isFirst === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).remove.get.remove.get.isFirst === true)

    assert(new MikeDoublyLinkedList[Int](5, None, None).isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).remove.get.isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).remove.get.append(7).isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).remove.get.append(7).remove.get.isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).append(7).isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).append(7).remove.get.isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).append(7).remove.get.remove.get.isLast === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).isFirst === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).isFirst === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).remove.get.isFirst === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).remove.get.append(7).isFirst === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).remove.get.append(7).remove.get.isFirst === true)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).append(7).isFirst === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).append(7).remove.get.isFirst === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).append(7).remove.get.remove.get.isFirst === true)

    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).isLast === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).isLast === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).prepend(8).isLast === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).isFirst === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).isFirst === false)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).prepend(9).isFirst === false)
  }

  test("size returns number of elements in linked list") {
    assert(new MikeDoublyLinkedList[Int](5, None, None).size === 1)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).size === 2)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).size === 3)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).size === 4)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.size === 2)

    assert(new MikeDoublyLinkedList[Int](5, None, None).size === 1)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).size === 2)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).append(7).size === 3)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).append(7).append(8).size === 4)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).append(7).append(8).remove.get.remove.get.size === 2)

    assert(new MikeDoublyLinkedList[Int](5, None, None).size === 1)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).size === 2)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).size === 3)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).size === 4)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).append(9).size === 5)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).append(9).prepend(10).size === 6)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).size === 4)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).remove.get.remove.get.size === 2)

    assert(new MikeDoublyLinkedList[Int](5, None, None).size === 1)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).size === 2)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).size === 3)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).size === 4)
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).remove.get.remove.get.size === 2)
  }

  test("Linked list can be expressed as a string") {
    assert(new MikeDoublyLinkedList[Int](5, None, None).toString === "5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).toString === "6 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).toString === "5 <-> 6")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).toString === "7 <-> 6 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).append(7).toString === "5 <-> 6 <-> 7")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).toString === "6 <-> 7 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).append(8).toString === "7 <-> 8 <-> 6 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).toString === "5 <-> 7 <-> 6")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).toString === "8 <-> 7 <-> 6 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).toString === "6 <-> 8 <-> 7 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.toString === "6 <-> 5")

    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).toString === "6 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).toString === "6 <-> 7 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).toString === "6 <-> 7 <-> 8 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).append(9).toString === "6 <-> 7 <-> 8 <-> 9 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).toString === "6 <-> 8 <-> 7 <-> 5")
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).append(9).toString === "6 <-> 8 <-> 9 <-> 7 <-> 5")

    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).toString === "5 <-> 6")
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).toString === "5 <-> 7 <-> 6")
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).toString === "5 <-> 7 <-> 8 <-> 6")
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).prepend(8).toString === "5 <-> 8 <-> 7 <-> 6")
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).toString === "5 <-> 7 <-> 8 <-> 6")
    assert(new MikeDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).prepend(9).toString === "5 <-> 7 <-> 9 <-> 8 <-> 6")
  }

  test("last returns last node in the list") {
    assert(new MikeDoublyLinkedList[Int](5, None, None).last.item === 5)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).last.item === 5)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).last.item === 5)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).last.item === 5)
    assert(new MikeDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.last.item === 5)
  }
}
