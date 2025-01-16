package Task1

import scala.util.Random

class Client(private val atm: ATM) extends Thread {
  private val max_amount: Int = 100
  private val id = Client.generateNextId()
  
  
  override def run(): Unit = {
    val amount = Random.nextInt(max_amount) + 1
    
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