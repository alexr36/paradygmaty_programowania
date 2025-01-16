package Zad_2

class Main

object Main extends App {
  val buffer = new Buffer
  val pi_evaluator = new PiEvaluator(buffer, "Pi Evaluator")
  val factor_calculator = new FactorCalculator(buffer, "Factor Calculator")

  pi_evaluator.start()
  factor_calculator.start()
}
