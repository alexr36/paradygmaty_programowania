package Zad_2


//  Implementation of a unipartite buffer
class Buffer {
  private var value: Double = _
  private var is_empty: Boolean = true


  def put(toPut: Double): Unit = synchronized {
    while (!is_empty) wait()

    value = toPut
    is_empty = false
    notifyAll()
  }


  def get(): Double = synchronized {
    while (is_empty) wait()

    is_empty = true
    notifyAll()
    value
  }
}
