package MikeDataStruct

import scala.annotation.tailrec

class MikeMutableDoublyLinkedList[A] {
  case class MikeMutableDoublyLinkedListEl[A](item: A,
                                              var prev: Option[MikeMutableDoublyLinkedListEl[A]],
                                              var next: Option[MikeMutableDoublyLinkedListEl[A]])

  private var first: Option[MikeMutableDoublyLinkedListEl[A]] = None
  private var last: Option[MikeMutableDoublyLinkedListEl[A]] = None
  private var len: Long = 0
  def length: Long = len

  override def toString: String = toString(" <-> ")
  def toString(connector: String): String = {
    @tailrec def toStringAcc(node: MikeMutableDoublyLinkedListEl[A], acc: String): String =
      if (node.next.isEmpty) acc + node.item.toString else toStringAcc(node.next.get, acc + node.item.toString + connector)
    if (first.isEmpty) "" else toStringAcc(first.get, "")
  }

  private def getFirst: Option[MikeMutableDoublyLinkedListEl[A]] = first
  private def getLast: Option[MikeMutableDoublyLinkedListEl[A]] = last

  private def getNextToSpecified(item2: A): Option[MikeMutableDoublyLinkedListEl[A]] = {
    @tailrec def getNextToSpecifiedAcc(acc: MikeMutableDoublyLinkedListEl[A]): Option[MikeMutableDoublyLinkedListEl[A]] = {
      if (acc.next.isEmpty) None else if (acc.next.get.item == item2) Some(acc) else getNextToSpecifiedAcc(acc.next.get)
    }
    if (first.isEmpty || first.get.item == item2) None else getNextToSpecifiedAcc(first.get)
  }

  def peek: Option[A] = if (first.isEmpty) None else Some(first.get.item)

  def push(item2: A): Unit = {
    val el = new MikeMutableDoublyLinkedListEl[A](item2, None, first)
    if (len == 0) {
      last = Some(el)
    } else {
      first.get.prev = Some(el)
    }
    first = Some(el)
    len = len + 1
  }

  def pop: Option[A] = {
    if (len == 0) None
    else {
      val item = first.get.item
      if (len == 1) {
        first = None
        last = None
      } else {
        first = first.get.next
        first.get.prev = None
      }
      len = len - 1
      Some(item)
    }
  }

  def peekLast: Option[A] = if (last.isEmpty) None else Some(last.get.item)

  def pushLast(item2: A): Unit = {
    val el = new MikeMutableDoublyLinkedListEl[A](item2, last, None)
    if (len == 0) {
      first = Some(el)
    } else {
      last.get.next = Some(el)
    }
    last = Some(el)
    len = len + 1
  }

  def popLast: Option[A] = {
    if (len == 0) None
    else {
      val item = last.get.item
      if (len == 1) {
        first = None
        last = None
      } else {
        last = last.get.prev
        last.get.next = None
      }
      len = len - 1
      Some(item)
    }
  }

  def insertBefore(item2: A, before: A): Option[A] = {
    if (first.isEmpty) None
    else if (first.get.item == before) {
      push(item2)
      Some(item2)
    }
    else {
      val nextToSpecified = getNextToSpecified(before)
      if (nextToSpecified.isEmpty) None
      else {
        val el = new MikeMutableDoublyLinkedListEl[A](item2, nextToSpecified, nextToSpecified.get.next)
        nextToSpecified.get.next = Some(el)
        if (el.next.isEmpty) el.next.get.prev = Some(el)
        len = len + 1
        Some(el.item)
      }
    }
  }

  def remove(item2: A): Option[A] = {
    if (len == 0) None
    else if (first.get.item == item2) pop
    else if (last.get.item == item2) popLast
    else {
      val nextToSpecified = getNextToSpecified(item2)
      if (nextToSpecified.isEmpty) None
      else {
        // Element el is what we are removing.
        val el = nextToSpecified.get.next
        // At this point we know that el.get.next is not empty because we know that it's not "last"
        // because if it were last, we would've triggered poplast above instead of being here.
        nextToSpecified.get.next = el.get.next
        el.get.next.get.prev = nextToSpecified
        len = len - 1
        Some(el.get.item)
      }
    }
  }

  def reverse(): Unit = {
    // Nothing to do if it's a 0- or 1-element list
    if (first.nonEmpty && first.get.next.nonEmpty) {
      def reverseAcc(curr: Option[MikeMutableDoublyLinkedListEl[A]]): Unit = {
        if (curr.isEmpty) {
          val temp = last
          first = last
          last = temp
          first.get.prev = None
          last.get.next = None
        }
        else {
          val temp = curr.get.next
          curr.get.next = curr.get.prev
          curr.get.prev = temp
          reverseAcc(temp)
        }
      }
      reverseAcc(first)
    }
  }

  def contains(item2: A): Boolean = {
    @tailrec def containsAcc(acc: MikeMutableDoublyLinkedListEl[A]): Boolean = {
      if (acc.item == item2) true else if (acc.next.isEmpty) false else containsAcc(acc.next.get)
    }
    if (first.isEmpty) false else containsAcc(first.get)
  }
}
