package Task2

class QuotientCalculator(private val name: String, private val buffer: Buffer) extends Thread(name) {
  override def run(): Unit = {
    var n = 1
    
    while (n > 0) {
      println(s"Iteration: $n")
      
      val numerator = 4.0 * n * n             //  n = 4n^2
      val denominator = 4.0 * n * n - 1.0     //  d = 4n^2 - 1
      val quotient = numerator / denominator  //  q = (4n^2) / (4n^2 - 1)
      
      buffer.put(quotient)
      
      n += 1
    }
  }
}
