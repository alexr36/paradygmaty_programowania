//  ZADANIE 2

//  ==  Definicje cech  ================================================================================================

//  Latanie
trait Flying {
  def flyingDescription(): String = "Latam"
}

trait WeakFlying extends Flying {
  override def flyingDescription(): String = "Slabo latam"
}

trait GoodFlying extends Flying {
  override def flyingDescription(): String = "Dobrze latam"
}

trait GreatFlying extends Flying {
  override def flyingDescription(): String = "Swietnie latam"
}

trait ExcellentFlying extends Flying {
  override def flyingDescription(): String = "Latam znakomicie"
}


//  Nurkowanie
trait Diving {
  def divingDescription(): String = "Nurkuje"
}


//  Pływanie
trait Swimming {
  def swimmingDescription(): String = "Plywam"
}


//  Bieganie
trait Running {
  def runningDescription(): String = "Biegam"
}

trait WeakRunning extends Running {
  override def runningDescription(): String = "Slabo biegam"
}

trait GoodRunning extends Running {
  override def runningDescription(): String = "Dobrze biegam"
}

trait GreatRunning extends Running {
  override def runningDescription(): String = "Swietnie biegam"
}

trait ExcellentRunning extends Running {
  override def runningDescription(): String = "Biegam znakomicie"
}


//  ==  Klasa abstrakcyjna Bird  =======================================================================================

abstract class Bird(private val _species: String) {
  private val _id: Int = Bird.generateId()

  
  //  Komunikat triumfalny
  println("Pochodze od dinozaurow!!!")

  
  override def toString: String = s"Ptak nr $_id - $_species"
}

//  Obiekt towarzyszący klasy Bird
object Bird {
  private var next_id = 1

  
  //  Generowanie unikalnego numeru ewidencyjnego
  private def generateId(): Int = {
    val id = next_id
    next_id += 1
    id
  }
}


//  ==  Podklasy  ======================================================================================================

//  Podklasa Penguin
class Penguin extends Bird("Pingwin") with Swimming with Diving

//  Podklasa Pigeon
class Pigeon extends Bird("Golab") with GoodFlying with WeakRunning with Swimming

//  Podklasa Ostrich
class Ostrich extends Bird("Strus") with GreatRunning

//  Podklasa Hen
class Hen extends Bird("Kura") with GoodRunning with WeakFlying

//  Podklasa Falcon
class Falcon extends Bird("Sokol") with ExcellentFlying


//  ==  Testy ==========================================================================================================

// Tworzenie ptaków
val pi1 = new Penguin
val go1 = new Pigeon
val st1 = new Ostrich
val so1 = new Falcon
val ku1 = new Hen
val pi2 = new Penguin
val go2 = new Pigeon
val st2 = new Ostrich

// Lista wszystkich ptaków
val birds: List[Bird] = List(pi1, go1, st1, so1, ku1, pi2, go2, st2)

// Listy ptaków według cech
val swimmingBirds = birds.collect { case p: Swimming => p }
val divingBirds = birds.collect { case p: Diving => p }
val flyingBirds = birds.collect { case p: Flying => p }
val runningBirds = birds.collect { case p: Running => p }

// Wyświetlanie danych ptaków z każdej listy
println("\nPtaki latające:")
flyingBirds.foreach(bird =>
  println(s"$bird\n${bird.flyingDescription()}")
)

println("\nPtaki nurkujące:")
divingBirds.foreach(bird =>
  println(s"$bird\n${bird.divingDescription()}"))

println("Ptaki pływające:")
swimmingBirds.foreach(bird =>
  println(s"$bird\n${bird.swimmingDescription()}"))

println("\nPtaki biegające:")
runningBirds.foreach(bird =>
  println(s"$bird\n${bird.runningDescription()}")
)