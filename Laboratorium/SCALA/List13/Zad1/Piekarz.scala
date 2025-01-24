package List13.Zad1

class Piekarz(private val bakery: Piekarnia) extends Thread {
  override def run(): Unit = {
    while (true) {
      println(s"[Baker] Baking bread...")
      bakery.bakeBread()
      Thread.sleep(2000)    //  Sleep for 2s
    }
  }
}
