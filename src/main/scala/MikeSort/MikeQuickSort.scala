package MikeSort

import scala.reflect.ClassTag

class MikeQuickSort[A: Ordering](order: MikeSortOrder.Value)(implicit m: ClassTag[A]) extends MikeSort[A](order: MikeSortOrder.Value) {
  def sort(xs: List[A]): List[A] = {
    val xsArray = xs.toArray[A]
    sort(xsArray, 0, xs.length - 1)
    xsArray.toList
  }

  def sort(xs: Array[A], lo: Int, hi: Int): Unit = {
    if (lo < hi) {
      val p = partition(xs, lo, hi)
      sort(xs, lo, p - 1)
      sort(xs, p + 1, hi)
    }
  }

  def partition(xs: Array[A], lo: Int, hi: Int): Int = {
    require(lo < hi, "List to be partitioned must be at least 2 elements long")
    // Take last element as the pivot
    val pivot: A = xs(hi)
    var i = lo - 1
    for (j <- lo until hi) {
      if (isBefore(xs(j), pivot)) {
        i = i + 1
        val temp = xs(j)
        xs(j) = xs(i)
        xs(i) = temp
      }
    }
    i = i + 1
    if (isBefore(pivot, xs(i))) {
      xs(hi) = xs(i)
      xs(i) = pivot
    }
    i
  }
}