package MikeSort

class MikeMergeSort[A: Ordering](order: MikeSortOrder.Value) extends MikeSort[A](order: MikeSortOrder.Value) {
  def sort(xs: List[A]): List[A] = {
    xs match {
      case Nil => xs
      case x :: Nil => xs
      case _ =>
        def merge(xs: List[A], ys: List[A]): List[A] = {
          (xs, ys) match {
            case (Nil, Nil) => List()
            case (Nil, ys2) => ys
            case (xs2, Nil) => xs
            case (x :: xs2, y :: ys2) =>
              if (isBefore(x, y)) x :: merge(xs2, ys)
              else y :: merge(xs, ys2)
          }
        }
        val (xs1, ys1) = xs.splitAt(xs.length / 2)
        merge(sort(xs1), sort(ys1))
    }
  }
}