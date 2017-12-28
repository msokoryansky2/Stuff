package TestMikeDataStruct

import scala.util.Random

import MikeDataStruct.{MikeMutableHeap, MikeMutableHeapType}
import org.scalatest.FunSuite

class MikeMutableHeapTest extends FunSuite {
  test("Can push and pop and print heap") {
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

    h.push(150)
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
        |....150
        |...4
        |..2
        |.100
      """.stripMargin.trim)

    assert(h.pop === Some(-1))
    assert(h.toString ===
      """
        |0
        |.1
        |..3
        |...3
        |....5
        |.....7
        |.....6
        |....150
        |...4
        |..2
        |.100
      """.stripMargin.trim)

    assert(h.pop === Some(0))
    assert(h.toString ===
      """
        |1
        |.2
        |..3
        |...3
        |....5
        |.....7
        |.....6
        |....150
        |...4
        |.100
      """.stripMargin.trim)

    assert(h.pop === Some(1))
    assert(h.toString ===
      """
        |2
        |.3
        |..3
        |...5
        |....7
        |....6
        |...150
        |..4
        |.100
      """.stripMargin.trim)

    assert(h.pop === Some(2))
    assert(h.toString ===
      """
        |3
        |.3
        |..5
        |...6
        |....7
        |...150
        |..4
        |.100
      """.stripMargin.trim)

    assert(h.pop === Some(3))
    assert(h.toString ===
      """
        |3
        |.4
        |..5
        |...6
        |....7
        |...150
        |.100
      """.stripMargin.trim)

    assert(h.pop === Some(3))
    assert(h.toString ===
      """
        |4
        |.5
        |..6
        |...7
        |..150
        |.100
      """.stripMargin.trim)

    assert(h.pop === Some(4))
    assert(h.toString ===
      """
        |5
        |.6
        |..7
        |..150
        |.100
      """.stripMargin.trim)

    assert(h.pop === Some(5))
    assert(h.toString ===
      """
        |6
        |.7
        |..150
        |.100
      """.stripMargin.trim)

    assert(h.pop === Some(6))
    assert(h.toString ===
      """
        |7
        |.150
        |.100
      """.stripMargin.trim)

    assert(h.pop === Some(7))
    assert(h.toString ===
      """
        |100
        |.150
      """.stripMargin.trim)

    assert(h.pop === Some(100))
    assert(h.toString ===
      """
        |150
      """.stripMargin.trim)

    assert(h.pop === Some(150))
    assert(h.toString === "")
  }

  test("random sequence of pushes results in an ordered pop") {
    val els = Seq.fill(10000)(Random.nextInt(1000) - 500).toList
    val elsSorted = els.sorted

    val h1 = new MikeMutableHeap[Int](MikeMutableHeapType.MIN)
    els.foreach(el => h1.push(el))
    var mins = (1 to els.size).map(i => h1.pop.get).toList
    assert(mins === elsSorted)

    val h2 = new MikeMutableHeap[Int](MikeMutableHeapType.MAX)
    els.foreach(el => h2.push(el))
    var maxs = (1 to els.size).map(i => h2.pop.get).toList
    assert(maxs === elsSorted.reverse)
  }
}
