package List13.Zad1

class Piekarnia(private val sufficient_loafs_amount: Int) {
  private val delivery_size = 3
  private val bags_to_loafs_ratio = 5
  private var current_loafs_amount = 0
  private var current_bags_of_flour = 0
  private val collections_max = 3
  private var collections_counter = 0
  private val lock = new Object

  //  Checking if the user passed a correct parameter value
  if (sufficient_loafs_amount <= 0) {
    throw new Exception(s"Wrong number of sufficient loafs: $sufficient_loafs_amount. " +
                        "It must be greater than 0.")
  }


  def bakeBread(): Unit = lock.synchronized {
    while (current_bags_of_flour <= 0) {
      println("[Baker] Waiting for the delivery of flour..." +
              s"\nCurrent amount of bags of flour: $current_bags_of_flour.")
      lock.wait()
    }

    current_loafs_amount += bags_to_loafs_ratio
    current_bags_of_flour -= 1

    println(s"[Baker] Bread has been baked successfully." +
      s"\nCurrent amount of loafs: $current_loafs_amount." +
      s"\nUsed 1 bag of flour. Current amount of bags of flour: $current_bags_of_flour.")
    lock.notifyAll()
  }


  def deliverFlour(): Unit = lock.synchronized {
    while (current_bags_of_flour > 0) {
      println(s"[Deliverer] Waiting for the deficit of flour... " +
              s"\nCurrent amount of bags of flour: $current_bags_of_flour.")
      lock.wait()
    }

    current_bags_of_flour += delivery_size
    println(s"[Provider] Flour has been delivered. " +
            s"\nCurrent amount of bags of flour: $current_bags_of_flour.")
    lock.notifyAll()
  }


  def collectBread(): Unit = lock.synchronized {
    while (current_loafs_amount < sufficient_loafs_amount) {
      println(s"[Collector] Waiting for enough bread to be baked... " +
              s"\nCurrent amount of loafs: $current_loafs_amount.")
      lock.wait()
    }

    current_loafs_amount -= sufficient_loafs_amount
    collections_counter += 1
    
    println(s"[Collector] $sufficient_loafs_amount loafs of bread have been collected successfully." +
            s"\nCurrent amount of loafs of bread: $current_loafs_amount. " +
            s"\n[Collector] Collections counter: $collections_counter.")
    
    endIfCollectedEnough()
    lock.notifyAll()
  }
  
  
  //  Checking if bread has been collected enough times - if yes then end program
  private def endIfCollectedEnough(): Unit = {
    if (collections_counter == collections_max) {
      println(s"[Collector] Completed collecting ordered amount of times: $collections_max. " +
        "Finishing the program...")
      System.exit(0)
    }
  }
}
