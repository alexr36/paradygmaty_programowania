package Task2

object Main extends App {
  val buffer = new Buffer()
  val qc = new QuotientCalculator("OC", buffer)
  val pe = new PiEvaluator("PE", buffer)

  qc.start()
  pe.start()
}
