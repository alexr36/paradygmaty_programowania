package List13.Zad2

class ListSegmentSumCalculator(private val list_segment: List[Int]) extends Thread {
  var partial_sum = 0

  override def run(): Unit = {
    for (i <- 0 to list_segment.length - 1) {
      partial_sum += list_segment(i)
    }
    
    println(s"Partial sum for segment: $list_segment: $partial_sum")
  }

  def getPartialSum: Int = partial_sum
}
