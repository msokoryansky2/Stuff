package MikeDataStruct

class MikeLinkedList[A](val item: A, val next: Option[MikeLinkedList[A]]) {
  def prepend(item2: A): MikeLinkedList[A] = new MikeLinkedList[A](item2, Some(this))
  def remove: Option[MikeLinkedList[A]] = next
  def isLast: Boolean = next.isEmpty
  def size: Long = {
    def sizeAcc(node: MikeLinkedList[A], acc: Long): Long = if (node.isLast) acc else sizeAcc(node.next.get, acc + 1)
    sizeAcc(this, 1)
  }
  override def toString: String = {
    def toStringAcc(node: MikeLinkedList[A], acc: String): String =
      if (node.isLast) acc + node.item.toString else toStringAcc(node.next.get, acc + node.item.toString + " -> ")
    toStringAcc(this, "")
  }
}

