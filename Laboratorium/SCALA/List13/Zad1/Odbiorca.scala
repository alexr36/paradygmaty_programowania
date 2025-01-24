package List13.Zad1

class Odbiorca(private val bakery: Piekarnia) extends Thread {
  override def run(): Unit = {
    while (true) {
      println("[Collector] Collecting bread...")
      bakery.collectBread()
      Thread.sleep(3000)    //  Sleep for 3s
    }
  }
}
