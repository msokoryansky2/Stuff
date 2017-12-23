package MikeDataStruct

import scala.annotation.tailrec

class MikeMutableLinkedList[A] {
  case class MikeMutableLinkedListEl[A](item: A, var next: Option[MikeMutableLinkedListEl[A]])

  private var first:  Option[MikeMutableLinkedListEl[A]] = None

  override def toString: String = toString(" -> ")
  def toString(connector: String): String = {
    @tailrec def toStringAcc(node: MikeMutableLinkedListEl[A], acc: String): String =
      if (node.next.isEmpty) acc + node.item.toString else toStringAcc(node.next.get, acc + node.item.toString + connector)
    if (first.isEmpty) "" else toStringAcc(first.get, "")
  }

  private def getFirst: Option[MikeMutableLinkedListEl[A]] = first

  private def getLast: Option[MikeMutableLinkedListEl[A]] = {
    @tailrec def getLastAcc(acc: MikeMutableLinkedListEl[A]): Option[MikeMutableLinkedListEl[A]] = {
      if (acc.next.isEmpty) Some(acc) else getLastAcc(acc.next.get)
    }
    if (first.isEmpty) None else getLastAcc(first.get)
  }

  private def getNextToLast: Option[MikeMutableLinkedListEl[A]] = {
    @tailrec def getNextToLastAcc(acc: MikeMutableLinkedListEl[A]): Option[MikeMutableLinkedListEl[A]] = {
      if (acc.next.get.next.isEmpty) Some(acc) else getNextToLastAcc(acc.next.get)
    }
    if (first.isEmpty || first.get.next.isEmpty) None else getNextToLastAcc(first.get)
  }

  private def getNextToSpecified(item2: A): Option[MikeMutableLinkedListEl[A]] = {
    @tailrec def getNextToSpecifiedAcc(acc: MikeMutableLinkedListEl[A]): Option[MikeMutableLinkedListEl[A]] = {
      if (acc.next.isEmpty) None else if (acc.next.get.item == item2) Some(acc) else getNextToSpecifiedAcc(acc.next.get)
    }
    if (first.isEmpty || first.get.item == item2) None else getNextToSpecifiedAcc(first.get)
  }

  def peek: Option[A] = if (first.isEmpty) None else Some(first.get.item)

  def push(item2: A): MikeMutableLinkedListEl[A] = {
    val el = new MikeMutableLinkedListEl[A](item2, first)
    first = Some(el)
    el
  }

  def pop: Option[A] = {
    if (first.isEmpty) None
    else {
      val item = first.get.item
      first = first.get.next
      Some(item)
    }
  }

  def peekLast: Option[A] = {
    val last = getLast
    if (last.isEmpty) None else Some(last.get.item)
  }

  def pushLast(item2: A): MikeMutableLinkedListEl[A] = {
    val el = new MikeMutableLinkedListEl[A](item2, None)
    if (first.isEmpty) first = Some(el) else getLast.get.next = Some(el)
    el
  }

  def popLast: Option[A] = {
    if (first.isEmpty) None
    else if (first.get.next.isEmpty) {
      val node = first.get
      first = None
      Some(node.item)
    }
    else {
      val node = getNextToLast
      val nodeLast = node.get.next
      node.get.next = None
      Some(nodeLast.get.item)
    }
  }

  def insertBefore(item2: A, before: A): Option[MikeMutableLinkedListEl[A]] = {
    if (first.isEmpty) None
    else {
      val nextToSpecified = getNextToSpecified(item2)
      if (nextToSpecified.isEmpty) None
      else {
        val someEl = Some(new MikeMutableLinkedListEl[A](item2, nextToSpecified.get.next))
        nextToSpecified.get.next = someEl
        someEl
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

  def contains(item2: A): Boolean = {
    @tailrec def containsAcc(acc: MikeMutableLinkedListEl[A]): Boolean = {
      if (acc.item == item2) true else if (acc.next.isEmpty) false else containsAcc(acc.next.get)
    }
    if (first.isEmpty) false else containsAcc(first.get)
  }

  def length: Long = {
    @tailrec def lengthAcc(node: MikeMutableLinkedListEl[A], acc: Long): Long = {
      if (node.next.isEmpty) acc else lengthAcc(node.next.get, acc + 1)
    }
    if (first.isEmpty) 0 else lengthAcc(first.get, 1)
  }
}
