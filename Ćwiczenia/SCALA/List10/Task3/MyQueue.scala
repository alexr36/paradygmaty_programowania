package Task3

//  ZADANIE 3

class MyQueue[+T] private (private val front: List[T], private val rear: List[T]) {
  def enqueue[S >: T](elem: S): MyQueue[S] = {
    new MyQueue(front, elem :: rear).normalize
  }


  def dequeue: MyQueue[T] = front match {
    case _ :: tail => new MyQueue(tail, rear).normalize
    case Nil => throw new NoSuchElementException("The queue is empty.")
  }


  def head: T = front match {
    case head :: _ => head
    case Nil => throw new NoSuchElementException("The queue is empty.")
  }


  def isEmpty: Boolean = front.isEmpty && rear.isEmpty


  override def toString: String = {
    (front ::: rear.reverse).mkString(", ")
  }


  private def normalize: MyQueue[T] = front match {
    case Nil => new MyQueue(rear.reverse, Nil)
    case _ => this
  }
}


object MyQueue {
  def empty[T] = new MyQueue[T](Nil, Nil)
  def apply[T](elements: T*) = new MyQueue[T](elements.toList, Nil)
}
