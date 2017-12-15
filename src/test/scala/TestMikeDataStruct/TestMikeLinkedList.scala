package TestMikeDataStruct

import MikeDataStruct.MikeLinkedList
import org.scalatest.FunSuite

class TestMikeLinkedList extends FunSuite {

  test("Single-node linked list is empty") {
    assert(new MikeLinkedList[Int](5, None).isLast === true)
    assert(new MikeLinkedList[Int](5, None).prepend(6).isLast === false)
    assert(new MikeLinkedList[Int](5, None).prepend(6).remove.get.isLast === true)
    assert(new MikeLinkedList[Int](5, None).prepend(6).remove.get.prepend(7).isLast === false)
    assert(new MikeLinkedList[Int](5, None).prepend(6).remove.get.prepend(7).remove.get.isLast === true)
    assert(new MikeLinkedList[Int](5, None).prepend(6).prepend(7).isLast === false)
    assert(new MikeLinkedList[Int](5, None).prepend(6).prepend(7).remove.get.isLast === false)
    assert(new MikeLinkedList[Int](5, None).prepend(6).prepend(7).remove.get.remove.get.isLast === true)
  }

  test("Linked list can be expressed as a string") {
    assert(new MikeLinkedList[Int](5, None).toString === "5")
    assert(new MikeLinkedList[Int](5, None).prepend(6).toString === "6 -> 5")
    assert(new MikeLinkedList[Int](5, None).prepend(6).prepend(7).toString === "7 -> 6 -> 5")
    assert(new MikeLinkedList[Int](5, None).prepend(6).prepend(7).prepend(8).toString === "8 -> 7 -> 6 -> 5")
    assert(new MikeLinkedList[Int](5, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.toString === "6 -> 5")
  }

  test("size returns number of elements in linked list") {
    assert(new MikeLinkedList[Int](5, None).size === 1)
    assert(new MikeLinkedList[Int](5, None).prepend(6).size === 2)
    assert(new MikeLinkedList[Int](5, None).prepend(6).prepend(7).size === 3)
    assert(new MikeLinkedList[Int](5, None).prepend(6).prepend(7).prepend(8).size === 4)
    assert(new MikeLinkedList[Int](5, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.size === 2)
  }
}
