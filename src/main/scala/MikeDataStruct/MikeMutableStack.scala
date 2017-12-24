package MikeDataStruct

class MikeMutableStack[A] {
  val l = new MikeMutableDoublyLinkedList[A]

  def push(item: A): Unit = l.push(item)
  def pop: Option[A] = l.pop
  def isEmpty: Boolean = l.length == 0
  def size: Long = l.length
}
