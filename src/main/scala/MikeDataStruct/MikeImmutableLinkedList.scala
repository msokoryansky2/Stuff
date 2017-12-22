package MikeDataStruct

import scala.annotation.tailrec

/**
  * Immutable singly-linked list impl
  */
class MikeImmutableLinkedList[A](val item: A, val next: Option[MikeImmutableLinkedList[A]]) {
  def prepend(item2: A): MikeImmutableLinkedList[A] = new MikeImmutableLinkedList[A](item2, Some(this))
  def remove: Option[MikeImmutableLinkedList[A]] = next
  def isLast: Boolean = next.isEmpty
  def last: MikeImmutableLinkedList[A] = if (isLast) this else next.get.last
  def size: Long = {
    @tailrec def sizeAcc(node: MikeImmutableLinkedList[A], acc: Long): Long = if (node.isLast) acc else sizeAcc(node.next.get, acc + 1)
    sizeAcc(this, 1)
  }
  override def toString: String = {
    @tailrec def toStringAcc(node: MikeImmutableLinkedList[A], acc: String): String =
      if (node.isLast) acc + node.item.toString else toStringAcc(node.next.get, acc + node.item.toString + " -> ")
    toStringAcc(this, "")
  }
}

