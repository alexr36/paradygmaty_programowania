import scala.annotation.tailrec
//  ZADANIE 1

//  ==  Klasa Zwierzak  ================================================================================================
class Zwierzak(private val _species: String = "default_species",
               private val _name: String = "default_name",
               private val _birth_year: Int = Int.MinValue) {

  //  Gettery
  def species: String = _species
  def name: String = _name
  def birth_year: Int = _birth_year


  def equals(other: Zwierzak): Boolean = {
    _species == other._species
      && _name == other._name
      && _birth_year == other._birth_year
  }


  override def toString: String = {
    s"[Gatunek: $_species; Imie: $_name; Rok urodzenia: $_birth_year]"
  }
}


//  ==  Klasa Obora  ===================================================================================================

class Obora(private val _owner: String = "default_owner",
            private var _stalls: Int = 0,
            private var _animals: List[Zwierzak] = List()) {

  private val _id: Int = Obora.generateId()


  //  Gettery
  def id: Int = _id
  def owner: String = _owner
  def stalls: Int = _stalls
  def animals: List[Zwierzak] = _animals


  //  Kwaterowanie zwierzaka w oborze
  def addAnimal(animal: Zwierzak): Unit = {
    if (_stalls > _animals.length) {
      _animals ::= animal
      println(s"Pomyslnie dodano zwierze $animal do obory $this.\n")
    }
    else {
      println(s"Nie udalo sie dodac zwierzecia do obory. Maksymalna liczba boksow: $_stalls.\n")
    }
  }


  //  Wykwaterowanie zwierzaka z obory
  def removeAnimal(animal: Zwierzak): Unit = {
    def aux(current_animals_list: List[Zwierzak]): List[Zwierzak] = {
      current_animals_list match
        case Nil => current_animals_list
        case head :: tail =>
          if (head.equals(animal)) aux(tail)
          else head :: aux(tail)
    }

    if (animal.equals(findAnimal(animal))) {
      println(s"Pomyslnie usunieto zwierze $animal z obory $this.\n")
    }

    _animals = aux(animals)
  }


  //  Przenoszenie zwierzaka do obory
  def moveAnimal(animal: Zwierzak, destination: Obora): Unit = {
    if (destination._stalls > destination._animals.length) {
      if (animal.equals(findAnimal(animal))) {
        removeAnimal(animal)
        destination.addAnimal(animal)
        println(s"Zakonczono przenoszenie zwierzecia z obory $this do obory $destination.\n")
      }
    }
    else {
      println(s"Nie mozna przeniesc zwierzecia do wybranej obory. Brak wolnych boksow.\n")
    }
  }


  //  Funkcja zwraca zadane zwierze, jeśli jest zarejestrowane w oborze
  def findAnimal(animal: Zwierzak): Zwierzak = {
    @tailrec
    def aux(animals_list: List[Zwierzak], found: Zwierzak): Zwierzak = {
      animals_list match
        case Nil => found
        case head :: tail =>
          if (head.equals(animal)) aux(tail, head)
          else aux(tail, found)
    }

    aux(_animals, null)
  }


  //  Wypisywanie listy zwierząt w oborze
  def showAnimals(): Unit = {
    println(s"Lista zwierzat w oborze $this:")
    _animals.foreach(animal => println(animal))
    println("\n")
  }


  override def toString: String = {
    s"[Wlasciciel: $_owner; Numer: $_id; Boksy: $_stalls; Zajete boksy: ${_animals.length}]"
  }
}


//  Obiekt towarzyszący
object Obora {
  private var next_id: Int = 1

  //  Generowanie unikalnego numeru obory
  private def generateId(): Int = {
    val id = next_id
    next_id += 1
    id
  }
}


//  ==  Testy ==========================================================================================================


