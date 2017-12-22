package TestMikeDataStruct

import MikeDataStruct.MikeImmutableLinkedList
import org.scalatest.FunSuite

class MikeImmutableLinkedListTest extends FunSuite {

  test("isLast and isFirst test if node is start or end of the list") {
    assert(new MikeImmutableLinkedList[Int](5, None).isLast === true)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).isLast === false)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).remove.get.isLast === true)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).remove.get.prepend(7).isLast === false)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).remove.get.prepend(7).remove.get.isLast === true)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).isLast === false)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).remove.get.isLast === false)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).remove.get.remove.get.isLast === true)
  }

  test("Linked list can be expressed as a string") {
    assert(new MikeImmutableLinkedList[Int](5, None).toString === "5")
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).toString === "6 -> 5")
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).toString === "7 -> 6 -> 5")
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).prepend(8).toString === "8 -> 7 -> 6 -> 5")
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.toString === "6 -> 5")
  }

  test("size returns number of elements in linked list") {
    assert(new MikeImmutableLinkedList[Int](5, None).size === 1)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).size === 2)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).size === 3)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).prepend(8).size === 4)
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.size === 2)
  }

  test("last returns last node in the list") {
    assert(new MikeImmutableLinkedList[Int](5, None).last.toString === "5")
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).last.toString === "5")
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).last.toString === "5")
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).prepend(8).last.toString === "5")
    assert(new MikeImmutableLinkedList[Int](5, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.last.toString === "5")
  }
}
