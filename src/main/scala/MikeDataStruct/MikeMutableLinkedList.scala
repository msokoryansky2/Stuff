package MikeDataStruct

import scala.annotation.tailrec

case class MikeMutableLinkedList[A <: AnyRef](item: A, var next: Option[MikeMutableLinkedList[A]])

class MikeMutableLinkedListBox[A <: AnyRef] {
  private var first:  Option[MikeMutableLinkedList[A]] = None

  override def toString: String = toString(" -> ")
  def toString(connector: String): String = {
    @tailrec def toStringAcc(node: MikeMutableLinkedList[A], acc: String): String =
      if (node.next.isEmpty) acc + node.item.toString else toStringAcc(node.next.get, acc + node.item.toString + connector)
    if (first.isEmpty) "" else toStringAcc(first.get, "")
  }

  private def getFirst: Option[MikeMutableLinkedList[A]] = first

  private def getLast: Option[MikeMutableLinkedList[A]] = {
    @tailrec def getLastAcc(acc: MikeMutableLinkedList[A]): Option[MikeMutableLinkedList[A]] = {
      if (acc.next.isEmpty) Some(acc) else getLastAcc(acc.next.get)
    }
    if (first.isEmpty) None else getLastAcc(first.get)
  }

  private def getNextToLast: Option[MikeMutableLinkedList[A]] = {
    @tailrec def getNextToLastAcc(acc: MikeMutableLinkedList[A],
                                  accNext: Option[MikeMutableLinkedList[A]]): Option[MikeMutableLinkedList[A]] = {
      if (accNext.isEmpty) Some(acc) else getNextToLastAcc(accNext.get, accNext.get.next)
    }
    if (first.isEmpty) None else getNextToLastAcc(first.get, first.get.next)
  }

  def peek: Option[A] = if (first.isEmpty) None else Some(first.get.item)

  def push(item2: A): MikeMutableLinkedList[A] = {
    val el = new MikeMutableLinkedList[A](item2, first)
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

  def pushLast(item2: A): MikeMutableLinkedList[A] = {
    val el = new MikeMutableLinkedList[A](item2, None)
    if (first.isEmpty) first = Some(el) else getLast.get.next = Some(el)
    el
  }

  def popLast: Option[A] = {
    if (first.isEmpty) None
    else {
      val node = getNextToLast
      node.get.next = None
      Some(node.get.item)
    }
  }

  def insertBefore(item2: A, before: A): Option[MikeMutableLinkedList[A]] = {
    if (first.isEmpty) None
    else {
      def insertBeforeAcc(last: Option[MikeMutableLinkedList[A]]): Option[MikeMutableLinkedList[A]] = {
        if (last.isEmpty) None
        else {
          val current = last.get
          if (before eq current.item) {
            val el = new MikeMutableLinkedList[A](item2, Some(current))
            last.get.next = Some(el)
            last.get.next
          } else {
            insertBeforeAcc(Some(current))
          }
        }
      }
      insertBeforeAcc(first)
    }
  }

  def contains(item2: A): Boolean = {
    @tailrec def containsAcc(acc: MikeMutableLinkedList[A]): Boolean = {
      if (acc.item eq item2) true else if (acc.next.isEmpty) false else containsAcc(acc.next.get)
    }
    if (first.isEmpty) false else containsAcc(first.get)
  }

  def length: Long = {
    @tailrec def lengthAcc(node: MikeMutableLinkedList[A], acc: Long): Long = {
      if (node.next.isEmpty) acc else lengthAcc(node.next.get, acc + 1)
    }
    if (first.isEmpty) 0 else lengthAcc(first.get, 1)
  }
}
