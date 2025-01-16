package Task2

class PiEvaluator(private val name: String, private val buffer: Buffer) extends Thread(name) {
  override def run(): Unit = {
    var curr_evaluation = 0.0
    var prev_product = 0.0
    var curr_product = 1.0
    val accuracy = 1e-10

    while (math.abs(curr_product - prev_product) > accuracy) {
      prev_product = curr_product
      curr_product *= buffer.get()
      curr_evaluation = curr_product * 2

      println(s"Evaluating...: $curr_evaluation")
    }

    println(s"Result: $curr_evaluation")
    System.exit(0)
  }
}
