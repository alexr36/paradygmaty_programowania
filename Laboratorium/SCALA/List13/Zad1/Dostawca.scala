package List13.Zad1

class Dostawca(private val bakery: Piekarnia) extends Thread {
  override def run(): Unit = {
    while (true) {
      println(s"[Deliverer] Delivering flour...")
      bakery.deliverFlour()
      Thread.sleep(4000) //  Sleep for 4s
    }
  }
}
