import Ordering.Implicits._

class MikeLinkedList[A: Ordering](val data: A, val next: Option[MikeLinkedList[A]]) {
  def insert(data2: A): MikeLinkedList[A] = new MikeLinkedList[A](data2, Some(this))
  def remove: Option[MikeLinkedList[A]] = next

  def last: MikeLinkedList[A] = if (next.isEmpty) this else next.last

  def insertLast(data2: A): MikeLinkedList[A] = last.insert(data2)

}

