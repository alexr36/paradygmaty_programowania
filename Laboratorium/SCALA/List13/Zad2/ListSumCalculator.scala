package List13.Zad2

class ListSumCalculator(private val threads_amount: Int, private val list: List[Int]) {
  private val segment_length = calculateSegmentLength()
  var sum = 0


  def calculateSum(): Int = synchronized {
    if (list.isEmpty) return 0

    val list_divided = divideList()

    list_divided.foreach(segment => handleSegment(segment))
    
    sum
  }


  private def calculateSegmentLength(): Int = {
    if (threads_amount <= 0) throw new RuntimeException("Number of threads should be greater than 0.")

    (threads_amount + list.length - 1) / threads_amount
  }


  private def divideList(): List[List[Int]] = {
    list.grouped(segment_length).toList
  }


  private def handleSegment(segment: List[Int]): Unit = {
    val list_segment_sum_calculator = new ListSegmentSumCalculator(segment)
    
    list_segment_sum_calculator.start()
    list_segment_sum_calculator.join()
 
    sum += list_segment_sum_calculator.getPartialSum
  }
}
