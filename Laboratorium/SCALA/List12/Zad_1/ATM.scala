package Zad_1

class ATM(private val min_balance: Int, private val refill_amount: Int) {
  private var current_balance = min_balance
  private val refill_delay = 3000  //  3s
  private val lock = new Object

  
  def refill(): Unit = lock.synchronized {
    while (current_balance < min_balance) {
      println(s"Refilling ATM... Current balance: $current_balance.")
      Thread.sleep(refill_delay)
      current_balance += refill_amount
      println(s"Refilled $refill_amount. Current balance: $current_balance.")

      lock.notify()
    }
  }


  def withdraw(amount: Int): Unit = lock.synchronized {
    while (current_balance < amount) {
      println(s"Cannot withdraw: $amount. Insufficient funds: $current_balance. Waiting for refill...")
      lock.wait()
    }

    current_balance -= amount
    println(s"Successfully withdrew: $amount. Current balance: $current_balance.")
  }
}
