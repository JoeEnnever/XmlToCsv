package learning

import collection.mutable.ListBuffer

/**
 *
 * <br>Date: 7/3/12
 * @author Joe Ennever
 */

class Queue[+T] private(
                         private[this] var leading: List[T],
                         private[this] var trailing: List[T]
                         ) {
  def this() = this(Nil, Nil)

  private def mirror() {
    if (leading.isEmpty) {
      while (!trailing.isEmpty) {
        leading = trailing.head :: leading
        trailing = trailing.tail
      }
    }
  }

  def head: T = {
    mirror(); leading.head
  }

  def tail: Queue[T] = {
    mirror()
    new Queue(leading.tail, trailing)
  }

  def enqueue[U >: T](t: U) = new Queue[U](leading, t :: trailing)

  private def elems = {
    mirror(); leading
  }

  override def toString = {
    mirror(); leading.mkString("Queue(", ",", ")")
  }
}

object Queue {
  def apply[T](xs: T*) = new Queue(xs.toList, Nil)

  def unapplySeq[T](q: Queue[T]): Option[Seq[T]] = if (q == null) None
  else {
    q.mirror(); Some(q.elems)
  }
}

