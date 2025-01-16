package Task1

object Main extends App {
  val atm = new ATM(250, 100)

  // Creating the queue manager
  val queueManager = new QueueManager(atm)

  queueManager.start()

  // Adding clients to the queue
  for (i <- 1 to 25) {
    val client = new Client(atm)
    queueManager.addClient(client)

    client.join()
  }

  println("\nAll clients used the ATM succesfully.")
  System.exit(0)
}