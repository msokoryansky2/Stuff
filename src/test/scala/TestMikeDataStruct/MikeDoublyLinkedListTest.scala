package TestMikeDataStruct

import MikeDataStruct.MikeMutableDoublyLinkedList
import org.scalatest.FunSuite

class MikeDoublyLinkedListTest extends FunSuite {

  assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).size === 3)

  test("isLast and isFirst test if node is start or end of the list") {
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).isLast === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.prepend(7).isLast === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.prepend(7).remove.get.isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).isLast === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).remove.get.isLast === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).remove.get.remove.get.isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).isFirst === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).isFirst === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.isFirst === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.prepend(7).isFirst === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).remove.get.prepend(7).remove.get.isFirst === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).isFirst === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).remove.get.isFirst === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).remove.get.remove.get.isFirst === true)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).remove.get.isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).remove.get.append(7).isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).remove.get.append(7).remove.get.isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).remove.get.isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).remove.get.remove.get.isLast === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).isFirst === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).isFirst === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).remove.get.isFirst === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).remove.get.append(7).isFirst === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).remove.get.append(7).remove.get.isFirst === true)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).isFirst === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).remove.get.isFirst === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).remove.get.remove.get.isFirst === true)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).isLast === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).isLast === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).prepend(8).isLast === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).isFirst === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).isFirst === false)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).prepend(9).isFirst === false)
  }

  test("size returns number of elements in linked list") {
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).size === 1)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).size === 2)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).size === 3)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).size === 4)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.size === 2)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).size === 1)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).size === 2)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).size === 3)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).append(8).size === 4)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).append(8).remove.get.remove.get.size === 2)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).size === 1)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).size === 2)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).size === 3)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).size === 4)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).append(9).size === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).append(9).prepend(10).size === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).size === 4)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).remove.get.remove.get.size === 2)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).size === 1)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).size === 2)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).size === 3)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).size === 4)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).remove.get.remove.get.size === 2)
  }

  test("Linked list can be expressed as a string") {
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).toString === "5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).toString === "6 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).toString === "5 <-> 6")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).toString === "7 <-> 6 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).toString === "5 <-> 6 <-> 7")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).toString === "6 <-> 7 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).append(8).toString === "7 <-> 8 <-> 6 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).toString === "5 <-> 7 <-> 6")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).toString === "8 <-> 7 <-> 6 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).toString === "6 <-> 8 <-> 7 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.toString === "6 <-> 5")

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).toString === "6 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).toString === "6 <-> 7 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).toString === "6 <-> 7 <-> 8 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).append(9).toString === "6 <-> 7 <-> 8 <-> 9 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).toString === "6 <-> 8 <-> 7 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).append(9).toString === "6 <-> 8 <-> 9 <-> 7 <-> 5")

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).toString === "5 <-> 6")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).toString === "5 <-> 7 <-> 6")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).toString === "5 <-> 7 <-> 8 <-> 6")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).prepend(8).toString === "5 <-> 8 <-> 7 <-> 6")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).toString === "5 <-> 7 <-> 8 <-> 6")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).prepend(9).toString === "5 <-> 7 <-> 9 <-> 8 <-> 6")
  }

  test("last and first return last and first node in the list") {
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.last.item === 5)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).first.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).first.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).first.item === 7)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).first.item === 8)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.first.item === 6)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).first.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).first.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).first.item === 7)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).first.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).first.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).append(8).first.item === 7)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).first.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).first.item === 8)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).first.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.first.item === 6)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).first.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).first.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).first.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).append(9).first.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).first.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).append(9).first.item === 6)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).first.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).first.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).prepend(8).first.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).first.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).prepend(9).first.item === 5)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).last.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).append(7).last.item === 7)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).append(8).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).last.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).prepend(7).prepend(8).remove.get.remove.get.last.item === 5)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).append(8).append(9).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).last.item === 5)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).prepend(6).append(7).prepend(8).append(9).last.item === 5)

    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).last.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).last.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).prepend(8).last.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).last.item === 6)
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).prepend(9).last.item === 6)
  }

  test("Can potentially remove every node in the list") {
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).remove.get.remove.get.remove.get.append(8).prepend(9).prepend(10).append(11).toString === "10 <-> 11 <-> 9 <-> 8")
  }

  test("reverse reverses a list") {
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).prepend(9).reverse.toString === "6 <-> 8 <-> 9 <-> 7 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).prepend(9).reverse.reverse.toString === "5 <-> 7 <-> 9 <-> 8 <-> 6")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).prepend(9).reverse.reverse.reverse.toString === "6 <-> 8 <-> 9 <-> 7 <-> 5")
    assert(new MikeMutableDoublyLinkedList[Int](5, None, None).append(6).prepend(7).append(8).prepend(9).reverse.reverse.reverse.reverse.toString === "5 <-> 7 <-> 9 <-> 8 <-> 6")
  }
}
