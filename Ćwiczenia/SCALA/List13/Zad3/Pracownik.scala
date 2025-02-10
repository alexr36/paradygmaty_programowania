package Zad3


class Pracownik(private var _nazwisko: String) {
  //  Class fields
  private var _zwolniony = false


  //  Increments the number of employees when a new one is created
  Pracownik.incrementEmployees()


  //  Public methods
  def zwolnij(): Unit = {
    this._zwolniony = true
    Pracownik.decrementEmployees()
  }

  
  override def toString: String = { s"Surname: $_nazwisko, Is fired: $_zwolniony" }


  //  Accessors
  def nazwisko: String = _nazwisko
  def zwolniony: Boolean = _zwolniony


  //  Mutators
  def nazwisko_=(new_surname: String): Unit = { _nazwisko = new_surname }

}


//  Companion object
object Pracownik {
  private var employees_number: Int = 0


  def incrementEmployees(): Int = {
    employees_number += 1
    employees_number
  }


  def decrementEmployees(): Int = {
    employees_number -= 1
    employees_number
  }


  def liczbaPracownikow(): Int = { Pracownik.employees_number }
}
