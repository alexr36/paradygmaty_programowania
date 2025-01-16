package Zad_1


class QueueManager(atm: ATM) extends Thread {
  private val queue = new scala.collection.mutable.Queue[Client]()
  private val lock = new Object


  def addClient(client: Client): Unit = lock.synchronized {
    queue.enqueue(client)
    client.start()
  }


  override def run(): Unit = {
    while (true) {
      atm.refill()
    }
  }
}
