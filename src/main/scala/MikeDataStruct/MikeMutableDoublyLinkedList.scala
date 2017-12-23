package MikeDataStruct

import scala.annotation.tailrec

class MikeMutableDoublyLinkedList[A] {
  case class MikeMutableDoublyLinkedListEl[A](item: A,
                                              var prev: Option[MikeMutableDoublyLinkedListEl[A]],
                                              var next: Option[MikeMutableDoublyLinkedListEl[A]])

  private var first: Option[MikeMutableDoublyLinkedListEl[A]] = None
  private var last: Option[MikeMutableDoublyLinkedListEl[A]] = None

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
    if (first.nonEmpty) first.get.prev = Some(el)
    first = Some(el)
  }

  def pop: Option[A] = {
    if (first.isEmpty) None
    else {
      val item = first.get.item
      first = first.get.next
      Some(item)
    }
  }

  def peekLast: Option[A] = if (last.isEmpty) None else Some(last.get.item)

  def pushLast(item2: A): Unit = {
    val el = new MikeMutableDoublyLinkedListEl[A](item2, last, None)
    if (last.nonEmpty) last.get.next = Some(el)
    last = Some(el)
  }

  def popLast: Option[A] = {
    if (last.isEmpty) None
    else {
      val item = last.get.item
      last = last.get.prev
      Some(item)
    }
  }

  def insertBefore(item2: A, before: A): Option[A] = {
    if (first.isEmpty) None
    else if (first.get.item == before) {
      val el = new MikeMutableDoublyLinkedListEl[A](item2, first)
      first = Some(el)
      Some(el.item)
    }
    else {
      val nextToSpecified = getNextToSpecified(before)
      if (nextToSpecified.isEmpty) None
      else {
        val el = new MikeMutableDoublyLinkedListEl[A](item2, nextToSpecified.get.next)
        nextToSpecified.get.next = Some(el)
        Some(el.item)
      }
    }
  }

  def remove(item2: A): Option[A] = {
    if (first.isEmpty) None
    else if (first.get.item == item2) pop
    else {
      val nextToSpecified = getNextToSpecified(item2)
      if (nextToSpecified.isEmpty) None
      else {
        val el = nextToSpecified.get.next
        nextToSpecified.get.next = el.get.next
        Some(el.get.item)
      }
    }
  }

  def reverse(): Unit = {
    // Nothing to do if it's a 0- or 1-element list
    if (first.nonEmpty && first.get.next.nonEmpty) {
      def reverseAcc(last: MikeMutableDoublyLinkedListEl[A], curr: Option[MikeMutableDoublyLinkedListEl[A]]): Unit = {
        if (curr.isEmpty) {
          first.get.next = None
          first = Some(last)
        }
        else {
          val next = curr.get.next
          curr.get.next = Some(last)
          reverseAcc(curr.get, next)
        }
      }
      reverseAcc(first.get, first.get.next)
    }
  }

  def contains(item2: A): Boolean = {
    @tailrec def containsAcc(acc: MikeMutableDoublyLinkedListEl[A]): Boolean = {
      if (acc.item == item2) true else if (acc.next.isEmpty) false else containsAcc(acc.next.get)
    }
    if (first.isEmpty) false else containsAcc(first.get)
  }

  def length: Long = {
    @tailrec def lengthAcc(node: MikeMutableDoublyLinkedListEl[A], acc: Long): Long = {
      if (node.next.isEmpty) acc else lengthAcc(node.next.get, acc + 1)
    }
    if (first.isEmpty) 0 else lengthAcc(first.get, 1)
  }
}
