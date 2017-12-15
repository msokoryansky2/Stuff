package MikeDataStruct

/**
  * Mutable doubly-linked list impl
  */
class MikeDoublyLinkedList[A](val item: A, var prev: Option[MikeDoublyLinkedList[A]], var next: Option[MikeDoublyLinkedList[A]]) {
  def prepend(item2: A): MikeDoublyLinkedList[A] = {
    val prependedNode = new MikeDoublyLinkedList[A](item2, prev, Some(this))
    if (!isFirst) prev.get.next = Some(prependedNode)
    prev = Some(prependedNode)
    prependedNode
  }
  def append(item2: A): MikeDoublyLinkedList[A] = {
    val appendedNode = new MikeDoublyLinkedList[A](item2, Some(this), next)
    if (!isLast) next.get.prev = Some(appendedNode)
    next = Some(appendedNode)
    appendedNode
  }
  def remove: Option[MikeDoublyLinkedList[A]] = {
    if (isFirst && isLast) {
      None
    } else if (isFirst) {
      next.get.prev = None
      next
    } else if (isLast) {
      prev.get.next = None
      prev
    } else {
      prev.get.next = next
      next.get.prev = prev
      next
    }
  }
  def isFirst: Boolean = prev.isEmpty
  def isLast: Boolean = next.isEmpty
  def first: MikeDoublyLinkedList[A] = if (isFirst) this else prev.get.first
  def last: MikeDoublyLinkedList[A] = if (isLast) this else next.get.last

  def size: Long = {
    def sizeAcc(node: MikeDoublyLinkedList[A], acc: Long): Long =
      if (node.isLast) acc else sizeAcc(node.next.get, acc + 1)
    sizeAcc(first, 1)
  }
  override def toString: String = {
    def toStringAcc(node: MikeDoublyLinkedList[A], acc: String): String =
      if (node.isLast) acc + node.item.toString else toStringAcc(node.next.get, acc + node.item.toString + " <-> ")
    toStringAcc(first, "")
  }
}
