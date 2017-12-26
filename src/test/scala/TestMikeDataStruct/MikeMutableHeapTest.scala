package TestMikeDataStruct

import MikeDataStruct.{MikeMutableHeap, MikeMutableHeapType}
import org.scalatest.FunSuite

class MikeMutableHeapTest extends FunSuite {
  test("Can push onto and print heap") {
    val h = new MikeMutableHeap[Int](MikeMutableHeapType.MIN)

    assert(h.toString === "")

    h.push(0)
    assert(h.toString === "0")

    h.push(1)
    assert(h.toString ===
      """
        |0
        |.1
      """.stripMargin.trim)

    h.push(2)
    assert(h.toString ===
      """
        |0
        |.1
        |.2
      """.stripMargin.trim)

    h.push(3)
    assert(h.toString ===
      """
        |0
        |.1
        |..3
        |.2
      """.stripMargin.trim)

    h.push(4)
    assert(h.toString ===
      """
        |0
        |.1
        |..3
        |..4
        |.2
      """.stripMargin.trim)

    h.push(5)
    assert(h.toString ===
      """
        |0
        |.1
        |..3
        |...5
        |..4
        |.2
      """.stripMargin.trim)

    h.push(6)
    assert(h.toString ===
      """
        |0
        |.1
        |..3
        |...5
        |...6
        |..4
        |.2
      """.stripMargin.trim)

    h.push(7)
    assert(h.toString ===
      """
        |0
        |.1
        |..3
        |...5
        |....7
        |...6
        |..4
        |.2
      """.stripMargin.trim)

    h.push(3)
    assert(h.toString ===
      """
        |0
        |.1
        |..3
        |...3
        |....5
        |.....7
        |....6
        |..4
        |.2
      """.stripMargin.trim)

    h.push(-1)
    assert(h.toString ===
      """
        |-1
        |.0
        |..1
        |...3
        |....3
        |.....5
        |......7
        |.....6
        |...4
        |..2
      """.stripMargin.trim)

    h.push(100)
    assert(h.toString ===
      """
        |-1
        |.0
        |..1
        |...3
        |....3
        |.....5
        |......7
        |.....6
        |...4
        |..2
        |.100
      """.stripMargin.trim)
  }
}
