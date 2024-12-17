//  ZADANIE 1

//  --  Implementacja klasy Time  --------------------------------------------------------------------------------------

class Time(private var _hour: Int):                                                                                     //  Klasa Time
  _hour =                                                                                                                 //  Niech wartość pola modyfikowalnego _hour jest równa:
    if (_hour < 0) 0                                                                                                        //  Jeśli wprowadzono wartość < 0, 0
    else _hour                                                                                                              //  W przeciwnym wypadku, wprowadzonej wartości

  def hour: Int = _hour                                                                                                   //  Akcesor dla _hour

  def hour_= (new_hour: Int): Unit =                                                                                      //  Mutator dla _hour
    if (new_hour < 0) _hour = 0                                                                                             //  Jeśli nowa wartość jest < 0, ustaw 0
    else _hour = new_hour                                                                                                   //  W przeciwnym wypadku, ustaw nową wartość

  override def toString: String =                                                                                         //  Nadpisanie metody toString
    s"$_hour"                                                                                                               //  Zwróć wartość _hour jako napis

end Time                                                                                                                //  Koniec klasy Time

//  --  Testy ----------------------------------------------------------------------------------------------------------

val time = Time(0)
println(time)
time.hour = 4
println(time)
time.hour = -2
println(time)

val time1 = Time(7)
println(time1)

val time2 = Time(-4)
println(time2)