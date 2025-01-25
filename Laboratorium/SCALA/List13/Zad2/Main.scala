package List13.Zad2

class Main

object Main extends App {
  val threads_amount = 4
  val test_list = List.range(1, 11)
  val list_sum_calculator = new ListSumCalculator(threads_amount, test_list)

  val total_sum = list_sum_calculator.calculateSum()
  println(s"Total list sum: $total_sum.")
}
