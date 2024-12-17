//  ZADANIE 2, podpunkt a)

//  --  Implementacja klasy Time  --------------------------------------------------------------------------------------

class Time(private var _hour: Int = 0, private var _minute: Int = 0):                                                   //  Klasa Time, kontruktor domyślnie ustawia wartości pól _hour i _minute na 0
  if (!isCorrectHour(_hour)) throw new IllegalArgumentException(s"Invalid hour value: $_hour.")                           //  Jeśli podana wartość dla godzin jest nieprawidłowa, zgłoś wyjątek
  if (!isCorrectMinute(_minute)) throw new IllegalArgumentException(s"Invalid minute value: $_minute.")                   //  Jeśli podana wartość dla minut jest nieprawidłowa, zgłoś wyjątek


  def hour = _hour                                                                                                        //  Akcesor dla _hour
  def hour_=(new_hour: Int): Unit =                                                                                       //  Mutator dla _hour
    if (isCorrectHour(new_hour)) _hour = new_hour                                                                           //  Jeśli podana wartość jest prawidłowa, przypisz ją do _hour
    else throw new IllegalArgumentException(s"Invalid hour value: $new_hour.")                                              //  W przeciwnym wypadku, zgłoś wyjątek


  def minute = _minute                                                                                                    //  Akcesor dla _minute
  def minute_=(new_minute: Int): Unit =                                                                                   //  Mutator dla _minute
    if (isCorrectMinute(new_minute)) _minute = new_minute                                                                   //  Jeśli podana wartość jest prawidłowa, przypisz ją do _minute
    else throw new IllegalArgumentException(s"Invalid hour value: $new_minute.")                                            //  W przeciwnym wypadku, zgłoś wyjątek


  def before(other: Time): Boolean =                                                                                      //  Definicja metody before(other: Time)
    if (_hour < other._hour) true                                                                                           //  Jeśli _hour ma mniejszą wartość niż other._hour, zwróć true
    else if (_hour == other._hour)                                                                                          //  Jeśli _hour ma tą samą wartość co other._hour,
      _minute < other._minute                                                                                                 //  Porównaj wartości _minute i other._minute
    else false                                                                                                              //  W przeciwnym wypadku, zwróć false


  override def toString: String =                                                                                         //  Nadpisanie metody toString
    f"$_hour%02d:$_minute%02d"                                                                                              //  Formatowanie napisu, by uzupełniać go zerami wiodącymi do osiągnięcia długości 2 znaków


  //  Metody pomocnicze
  private def isCorrectHour(hour: Int): Boolean = hour >= 0 && hour <= 23                                                 //  Sprawdź, czy zadana wartość hour mieści się w zakresie [0, 23]
  private def isCorrectMinute(minute: Int): Boolean = minute >= 0 && minute <= 59                                         //  Sprawdź, czy zadana wartość minute mieści się w zakresie [0, 59]

end Time                                                                                                                //  Koniec klasy Time

//  --  Testy ----------------------------------------------------------------------------------------------------------

val time1 = new Time()
println(time1)
time1.hour = 2
println(time1)
time1.minute = 42
println(time1)
time1.hour = 23
println(time1)
time1.minute = 61
println(time1)
time1.hour = -1
println(time1)

time1.before(new Time(12, 51))
time1.before(new Time(23, 59))
time1.before(time1)

val time2 = new Time(51, 2)
val time3 = new Time(5, 521)
val time4 = new Time(21, 15)
println(time4)