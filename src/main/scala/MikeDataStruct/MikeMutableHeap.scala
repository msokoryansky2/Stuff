package MikeDataStruct

import scala.util.Properties

object MikeMutableHeapType extends Enumeration {
  val MIN = Value(-1)
  val MAX = Value(1)
}

object MikeMutableHeapTraversalType extends Enumeration {
  val IN_ORDER =   Value(0)
  val PRE_ORDER =  Value(-1)
  val POST_ORDER = Value(1)
}

class MikeMutableHeap[A: Ordering](val heapType: MikeMutableHeapType.Value = MikeMutableHeapType.MIN) {
  case class MikeMutableHeapEl[A](var v: Option[A], var l: Option[MikeMutableHeapEl[A]], var r: Option[MikeMutableHeapEl[A]])

  private var root: Option[MikeMutableHeapEl[A]] = None

  def traverse(traversal: MikeMutableHeapTraversalType.Value,
               f: (List[MikeMutableHeapEl[A]], MikeMutableHeapEl[A]) => Unit): Unit = {
    def walk(ancestors: List[MikeMutableHeapEl[A]], el: Option[MikeMutableHeapEl[A]]): Unit =
      if (el.nonEmpty) {
        traversal match {
          case MikeMutableHeapTraversalType.PRE_ORDER =>
            f(ancestors, el.get)
            walk(el.get :: ancestors, el.get.l)
            walk(el.get :: ancestors, el.get.r)
          case MikeMutableHeapTraversalType.POST_ORDER =>
            walk(el.get :: ancestors, el.get.l)
            walk(el.get :: ancestors, el.get.r)
            f(ancestors, el.get)
          case _ => // IN_ORDER
            walk(el.get :: ancestors, el.get.l)
            f(ancestors, el.get)
            walk(el.get :: ancestors, el.get.r)
        }
      }
    walk(List(), root)
  }

  override def toString: String = {
    var str = ""
    traverse(MikeMutableHeapTraversalType.PRE_ORDER,
            (ancestors, el) => str += "." * ancestors.length + el.v.get + Properties.lineSeparator)
    str.trim
  }

  /**
    * Returns true if a should be above b in the heap
    */
  import scala.math.Ordering.Implicits._
  private def isAbove(a: A, b: A): Boolean =
    (a < b && heapType == MikeMutableHeapType.MIN) || (a > b && heapType == MikeMutableHeapType.MAX)

  private def heapify(): Unit = {
    // TODO
  }

  def push(v2: A): Unit =
    if (root.isEmpty || root.get.v.isEmpty) root = Some(new MikeMutableHeapEl[A](Some(v2), None, None))
    else {
      /**
        * Bubble down v2 knowing that it should be somewhere below acc.v
        */
      def pushAcc(acc: MikeMutableHeapEl[A]): Unit = {
        if (acc.l.isEmpty) acc.l = Some(new MikeMutableHeapEl[A](Some(v2), None, None))       // v2 becomes new l
        else if (acc.r.isEmpty) acc.r = Some(new MikeMutableHeapEl[A](Some(v2), None, None))  // v2 becomes new r
        else if (isAbove(acc.l.get.v.get, v2)) pushAcc(acc.l.get)                             // v2 goes below l
        else if (isAbove(acc.r.get.v.get, v2)) pushAcc(acc.r.get)                             // v2 goes below r
        else acc.l = Some(new MikeMutableHeapEl[A](Some(v2), acc.l, None))                    // v2 injected above l
      }
      if (isAbove(root.get.v.get, v2)) pushAcc(root.get)
      else root = Some(new MikeMutableHeapEl[A](Some(v2), root, None))
    }

  def peek: Option[A] = if (root.isEmpty || root.get.v.isEmpty) None else root.get.v
  def pop: Option[A] =
    if (root.isEmpty || root.get.v.isEmpty) None
    else {
      val item = root.get.v
      root.get.v = None
      heapify
      item
    }
}
