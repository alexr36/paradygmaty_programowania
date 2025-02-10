package Zad3

class Main


object Main extends App {
  def printEmployees(): Unit = {
    println(s"Employee 1: $emp_1")
    println(s"Employee 2: $emp_2")
    println(s"Employee 3: $emp_3")
  }

  def printEmployeesNumber(): Unit = {
    println(s"Current number of employees: ${ Pracownik.liczbaPracownikow() }")
  }


  //  Tests
  val emp_1 = new Pracownik("Kowalski")
  val emp_2 = new Pracownik("Stanczuk")
  val emp_3 = new Pracownik("Nowakowski")

  printEmployeesNumber()
  printEmployees()
  emp_2.zwolnij()
  printEmployeesNumber()
  printEmployees()
}