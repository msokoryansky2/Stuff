package MikeSort

object MikeSortOrder extends Enumeration {
  val ASC = Value(1)
  val DESC = Value(-1)
}

abstract class MikeSort[A: Ordering](order: MikeSortOrder.Value) {
  import scala.math.Ordering.Implicits._
  def sort(xs: List[A]): List[A]
  def isBefore(x: A, y: A): Boolean = if (order == MikeSortOrder.ASC) x < y else x > y
}
