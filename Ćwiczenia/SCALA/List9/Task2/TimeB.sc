//  ZADANIE 2, podpunkt b)

//  --  Implementacja klasy Time  --------------------------------------------------------------------------------------

class Time(private var _minutes_from_midnight: Int = 0):                                                                //  Klasa Time, konstruktor domyślnie ustawia wartość pola _minutes_from_midnight na 0
  if (!isCorrectTime(_minutes_from_midnight))                                                                             //  Jeśli podana wartość nie jest prawidłowa,
    throw new IllegalArgumentException(s"Invalid minutes value: $_minutes_from_midnight.")                                  //  Zgłoś wyjątek


  def minutes_from_midnight = _minutes_from_midnight                                                                      //  Akcesor dla _minutes_from_midnight
  def minutes_from_midnight_=(new_minutes_from_midnight: Int): Unit =                                                     //  Mutator dla _minutes_from_midnight
    if (isCorrectTime(new_minutes_from_midnight)) _minutes_from_midnight = new_minutes_from_midnight                        //  Jeśli podana wartość jest prawidłowa, przypisz ją do _minutes_from_midnight
    else throw new IllegalArgumentException(s"Invalid minutes value: $_minutes_from_midnight.")                             //  W przeciwnym wypadku, zgłoś wyjątek


  def before(other: Time): Boolean =                                                                                      //  Definicja metody before(other: Time)
    _minutes_from_midnight < other._minutes_from_midnight                                                                   //  Porównaj wartości _minutes_from_midnight i other._minutes_from_midnight


  override def toString: String =                                                                                         //  Nadpisanie metody toString
    val hours = _minutes_from_midnight / 60                                                                                 //  Oblicz wartość hours dzieląc liczbę minut przez 60
    val minutes = _minutes_from_midnight % 60                                                                               //  Oblicz wartość minutes biorąc resztę z dzielenia liczby minut przez 60

    f"$hours%02d:$minutes%02d"                                                                                              //  Formatowanie napisu, by uzupełniać go zerami wiodącymi do osiągnięcia długości 2 znaków


  //  Metody pomocnicze
  private def isCorrectTime(minutes: Int): Boolean = minutes >= 0 && minutes < 1440                                       //  Sprawdź, czy podana wartość mieści się w zakresie [0, 1440)

end Time                                                                                                                //  Koniec klasy Time

//  --  Testy ----------------------------------------------------------------------------------------------------------

val time1 = new Time()
println(time1)
time1.minutes_from_midnight = 120
println(time1)
time1.minutes_from_midnight = 162
println(time1)
time1.minutes_from_midnight = 1422
println(time1)
time1.minutes_from_midnight = 1440
println(time1)
time1.minutes_from_midnight = -1
println(time1)

time1.before(new Time(70))
time1.before(new Time(1439))
time1.before(time1)

val time2 = new Time(5142)
val time3 = new Time(521)
val time4 = new Time(1275)
println(time4)