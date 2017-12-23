package MikeDataStruct

/**
  * Mutable doubly-linked list impl
  */
class MikeMutableDoublyLinkedList[A](var item: A, var prev: Option[MikeMutableDoublyLinkedList[A]], var next: Option[MikeMutableDoublyLinkedList[A]]) {
  private var _first: MikeMutableDoublyLinkedList[A] = if (prev.nonEmpty) prev.get._first else this
  private var _last: MikeMutableDoublyLinkedList[A] = if (next.nonEmpty) next.get._last else this
  private var _isEmpty = false

  def set(other: MikeMutableDoublyLinkedList[A]): MikeMutableDoublyLinkedList[A] = {
    this.item = other.item
    this.prev = other.prev
    this.next = other.next
    this._first = other._first
    this._last = other._last
    this._isEmpty = other._isEmpty
    this
  }
  def prepend(item2: A): MikeMutableDoublyLinkedList[A] = {
    if (_isEmpty) set(new MikeMutableDoublyLinkedList[A](item2, None, None))
    else {
      val prependedNode = new MikeMutableDoublyLinkedList[A](item2, prev, Some(this))
      if (!isFirst) prev.get.next = Some(prependedNode)
      prev = Some(prependedNode)
      _first = prependedNode._first
      prependedNode
    }
  }
  def append(item2: A): MikeMutableDoublyLinkedList[A] = {
    if (_isEmpty) set(new MikeMutableDoublyLinkedList[A](item2, None, None))
    else {
      val appendedNode = new MikeMutableDoublyLinkedList[A](item2, Some(this), next)
      if (!isLast) next.get.prev = Some(appendedNode)
      next = Some(appendedNode)
      _last = appendedNode._last
      appendedNode
    }
  }
  def remove: Option[MikeMutableDoublyLinkedList[A]] = {
    require(!_isEmpty, "Cannot remove from an empty list")
    if (isFirst && isLast) {
      _isEmpty = true
      Some(this)
    } else if (isFirst) {
      next.get.prev = None
      next.get._first = next.get
      next
    } else if (isLast) {
      prev.get.next = None
      prev.get._last = prev.get
      prev
    } else {
      prev.get.next = next
      next.get.prev = prev
      next
    }
  }
  def reverse: MikeMutableDoublyLinkedList[A] = {
    val temp = _first
    _first = _last
    _last = temp
    this
  }

  def isFirst: Boolean = prev.isEmpty
  def isLast: Boolean = next.isEmpty
  def first: MikeMutableDoublyLinkedList[A] = _first
  def last: MikeMutableDoublyLinkedList[A] = _last
  def isEmpty: Boolean = _isEmpty

  def size: Long = {
    def sizeAcc(node: MikeMutableDoublyLinkedList[A], acc: Long): Long =
      if (node.isLast) acc else sizeAcc(node.next.get, acc + 1)
    sizeAcc(_first, 1)
  }
  override def toString: String = {
    def toStringAcc(node: MikeMutableDoublyLinkedList[A], acc: String): String =
      if (node.isLast) acc + node.item.toString else toStringAcc(node.next.get, acc + node.item.toString + " <-> ")
    toStringAcc(_first, "")
  }
}
