import scala.annotation.tailrec
//  LISTA 7

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

def evaluateEulerConstant(precision: Double): Double = {
  if (precision <= 0)                                                                                                   //  Jeśli zadana dokładność jest <= 0,
    throw new RuntimeException(s"Nie mozna obliczyc stalej z przyblizeniem mniejszym niz ani rownym 0: $precision.")    //  rzuć wyjątek
  else {                                                                                                                //  W przeciwnym wypadku,
    @tailrec
    def aux(current_sum: Double, current_eval: Double, i: Int): Double = {                                              //  Funkcja pomocnicza
      val next_term = 1.0 / i                                                                                           //  Oblicz następny wyraz ciągu ze wzoru 1 / i
      val next_sum = current_sum + next_term                                                                            //  Oblicz następną sumę ciągu
      val next_eval = next_sum - math.log(i)                                                                            //  Oblicz następne przubliżenie stałej ze wzoru (aktualna suma ciągu) - log(i)

      if (math.abs(next_eval - current_eval) < precision) next_eval                                                     //  Jeśli |(następne przybliżenie) - (obecne przybliżenie)| < dokładność, zwróć następne przybliżenie
      else aux(next_sum, next_eval, i + 1)                                                                              //  W przeciwnym wypadku, wywołaj funkcję pomocniczą dla zakutalizowanych danych
    }

    aux(0.0, 0.0, 1)                                                                                                    //  Wywołaj funkcję pomocniczą z wartościami początkowymi
  }
}

evaluateEulerConstant(1e-20)

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

def composeFunctionsFromListRec[A](list: List[A => A]): (A => A) = {
  @tailrec
  def aux(current_list: List[A => A], result_function: (A => A)): (A => A) = {                                          //  Funkcja pomocnicza
    current_list match {                                                                                                //  Dopasuj listę funkcji do wzorca
      case Nil => result_function                                                                                       //  Jeśli lista jest pusta, zwróć wynikową funkcję
      case head :: tail => aux(tail, result_function compose head)                                                      //  Jeśli lista nie jest pusta, wykonaj złożenie wynikowej funkcji z kolejną funkcją z listy
    }
  }

  aux(list, (x: A) => x)                                                                                                //  Wywołanie funkcji pomocniczej z parametrami startowymi
}


def composeFunctionsFromListFunc[A](list: List[A => A]): (A => A) = {
  list.foldLeft((x: A) => x)((result_func, current_func) => result_func compose current_func)                           //
}


val functions = List(
  (x: Double) => x + 1,
  (x: Double) => x * 2,
  (x: Double) => x - 3
)

composeFunctionsFromListRec(functions)(5)
composeFunctionsFromListFunc(functions)(5)



