package MikeDataStruct

class MikeMutableQueue[A] {
  val l = new MikeMutableDoublyLinkedList[A]

  def enqueue(item: A): Unit = l.push(item)
  def dequeue: Option[A] = l.popLast
  def isEmpty: Boolean = l.length == 0
  def size: Long = l.length
}
