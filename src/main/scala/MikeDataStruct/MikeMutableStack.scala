package MikeDataStruct

class MikeMutableStack[A](items: MikeMutableDoublyLinkedList[A]) {
  def push(item: A): MikeMutableQueue[A] = ???
  def pop: Option[A] = ???
  def isEmpty: Boolean = ???
  def size: Long = ???
}
