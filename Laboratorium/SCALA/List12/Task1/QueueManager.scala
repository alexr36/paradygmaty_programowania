package Task1

import scala.collection.mutable.Queue

class QueueManager(atm: ATM) extends Thread {
  private val queue = new Queue[Client]()
  private val lock = new Object
  
  
  def addClient(client: Client): Unit = lock.synchronized {
    queue.enqueue(client)
    client.start()
  }


  def clientServed(): Unit = lock.synchronized {
    if (queue.nonEmpty) {
      val client = queue.dequeue()
      println(s"Client ${client.clientId} has been served and removed from the queue.")
    }
  }


  override def run(): Unit = {
    while (true) {
      atm.refill()
    }
  }
}