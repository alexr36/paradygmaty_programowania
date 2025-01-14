package Task3

class Main

object Main extends App {
  val queue = MyQueue(4, 6, 7, 1, 1, 15)

  println(queue)
  println(queue.dequeue)
  println(queue.enqueue(57))
}
