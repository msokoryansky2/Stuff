package MikeSortTest

import MikeSort.{MikeQuickSort, MikeSortOrder}
import org.scalatest.FunSuite

import scala.util.Random

class MikeQuickSortTest extends FunSuite {
  test("quick sort") {
    val s = new MikeQuickSort[Int](MikeSortOrder.ASC)

    assert(s.sort(List()) === List())
    assert(s.sort(List(1)) === List(1))
    assert(s.sort(List(1, 2)) === List(1, 2))
    assert(s.sort(List(1, 2, 3)) === List(1, 2, 3))
    assert(s.sort(List(1, 2, 3, 4)) === List(1, 2, 3, 4))
    assert(s.sort(List(6, 1, 5, 2, 3, 7, 4, 9, 8)) === List(1, 2, 3 ,4, 5, 6, 7, 8, 9))

    val s2 = new MikeQuickSort[Int](MikeSortOrder.DESC)

    assert(s2.sort(List()) === List().reverse)
    assert(s2.sort(List(1)) === List(1).reverse)
    assert(s2.sort(List(1, 2)) === List(1, 2).reverse)
    assert(s2.sort(List(1, 2, 3)) === List(1, 2, 3).reverse)
    assert(s2.sort(List(1, 2, 3, 4)) === List(1, 2, 3, 4).reverse)
    assert(s2.sort(List(6, 1, 5, 2, 3, 7, 4, 9, 8)) === List(1, 2, 3 ,4, 5, 6, 7, 8, 9).reverse)
  }

  test("random quick sort tests") {
    (1 to 100).foreach(n => {
      val order = if (Random.nextInt % 2 == 0) MikeSortOrder.ASC else MikeSortOrder.DESC
      val s = new MikeQuickSort[Float](order)

      val l = (1 to Random.nextInt(1000)).map(n => Random.nextFloat() * 1000 - 500).toList
      val lSorted = s.sort(l)

      if (order == MikeSortOrder.ASC) assert(l.sorted === lSorted) else assert(l.sorted.reverse === lSorted)
    })
  }
}
