package MikeDataStruct

class MikeQueue[A](val items: MikeDoublyLinkedList[A]) {
  def enqueue(item: A): MikeQueue[A] = ???
  def dequeue: Option[A] = ???
  def isEmpty: Boolean = ???
  def size: Long = ???
}
