package MikeDataStruct

class MikeMutableQueue[A](val items: MikeMutableDoublyLinkedList[A]) {
  def enqueue(item: A): MikeMutableQueue[A] = ???
  def dequeue: Option[A] = ???
  def isEmpty: Boolean = ???
  def size: Long = ???
}
