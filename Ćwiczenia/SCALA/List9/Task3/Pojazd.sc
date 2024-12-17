//  ZADANIE 3

//  --  Implementacja klasy Pojazd -------------------------------------------------------------------------------------

class Pojazd(private val _manufacturer: String,                                                                         //  Klasa Pojazd
             private val _model: String,
             private val _year: Int = -1,
             private var _id: String = ""):

  def this(manufacturer: String , model: String) =                                                                      //  Pierwszy konstruktor pomocniczy
    this(manufacturer, model, -1, "")

  def this(manufacturer: String, model: String, id: String) =                                                           //  Drugi konstruktor pomocinczy
    this(manufacturer, model, -1, id)

  def this(manufacturer: String, model: String, year: Int) =                                                            //  Trzeci konstruktor pomocniczy
    this(manufacturer, model, year, "")

  override def toString: String =                                                                                       //  Nadpisanie metody toString
    s"[Producent: $_manufacturer; Model: $_model; Rok: $_year; ID: $_id]"

end Pojazd                                                                                                              //  Koniec klasy Pojazd

//  --  Testy ----------------------------------------------------------------------------------------------------------

val p1 = new Pojazd("Toyota", "Corolla")
val p2 = new Pojazd("Honda", "Civic", 2020)
val p3 = new Pojazd("Ford", "Focus", "ABC1234")
val p4 = new Pojazd("BMW", "X5", 2018, "XYZ5678")

println(p1)
println(p2)
println(p3)
println(p4)
