package Zad_1


class Client(private val atm: ATM, private val max_amount: Int) extends Thread {
  private val id = Client.generateNextId()


  override def run(): Unit = {
    val amount = scala.util.Random.nextInt(max_amount) + 1

    println(s"Client $id is attempting to withdraw $amount.")
    atm.withdraw(amount)
    println(s"Client $id successfully withdrew $amount.")
  }


  def clientId: Int = id
}


// Companion object for Client class
object Client {
  private var next_id: Int = 1

  def generateNextId(): Int = synchronized {
    val id = next_id
    next_id += 1
    id
  }
}
