package Zad_1


class Main


object Main extends App {
  val min_atm_balance = 250
  val atm = new ATM(min_atm_balance, 100)
  
  val queueManager = new QueueManager(atm)
  queueManager.start()

  // Adding clients to the queue
  for (i <- 1 to 25) {
    val client = new Client(atm, min_atm_balance)
    queueManager.addClient(client)

    client.join()
  }

  println("\nAll clients used the ATM succesfully.")
  System.exit(0)
}
