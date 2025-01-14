package Task3

class Main

object Main extends App {
  val queue = MyQueue(4, 6, 7, 1, 1, 15)

  println(queue)                //  4, 6, 7, 1, 1, 15
  println(queue.dequeue)        //  6, 7, 1, 1, 15
  println(queue.enqueue(57))    //  4, 6, 7, 1, 1, 15, 57
  println(queue.first)          //  4
}
