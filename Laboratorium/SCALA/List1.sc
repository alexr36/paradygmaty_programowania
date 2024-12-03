//  LISTA 1

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

def buildTriangle(a: Double, b: Double, c: Double): Double = {
  if (a <= 0 || b <= 0 || c <= 0)                                                                                       //  Jeśli ktorykolwiek z bokow jest <= 0, rzuc wyjątek
    throw new RuntimeException(s"Nie mozna skonstruowac trokjata o bokach dlugosci mniejszej lub rownej 0: $a, $b, $c.")
  else if (math.abs(b - c) < a && a < b + c) {                                                                          //  Jeśli warunek |b - c| < a < b + c jest spełniony:
      val p = (a + b + c) / 2                                                                                           //  Niech p = (a + b + c) / 2
      math.sqrt(p * (p - a) * (p - b) * (p - c))                                                                        //  Oblicz i zwroc pole trojkąta ze wzoru Herona
  }
  else throw new RuntimeException(s"Nie mozna zbudowac trojkata o dlugosciach bokow: $a, $b, $c.")                      //  W przeciwnym wypadku, rzuc wyjątek
}


buildTriangle(2.0, 3.0, 4.0)
buildTriangle(8.0, 2.0, 1.0)
buildTriangle(-1.0, 5.0, 0.0)

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

def calculateSum(n: Int, x: Double): Double = {
  if (n <= 0) throw new RuntimeException(s"Nie mozna obliczyc wyrazu ciagu o indeksie mniejszym lub rownym 0: $n")      //  Jeśli zadane n jest <= 0, rzuć wyjątek
  else {
    def aux(i: Int, current_sum: Double, current_term: Double): Double = {                                              //  Funkcja pomocnicza
      if (i > n) current_sum                                                                                            //  Jeśli i > n, zwroc aktualną sume
      else {                                                                                                            //  W przeciwnym wypadku
        val next_term = current_term * -1.0 * x / i                                                                     //  Oblicz następny wyraz ciagu ze wzoru (-1)^i x^i / i!
        val next_sum = current_sum + next_term                                                                          //  Zaktualizuj aktualną sume

        aux(i + 1, next_sum, next_term)                                                                                 //  Wywolaj funkcje pomocnicza dla zaktualizowanych parametrow: inkrementowane i, aktualna suma, nastepny wyraz
      }
    }

    aux(1, 0.0, 1.0)                                                                                                    //  Wywolaj funkcje pomocnicza dla parametrow startowych: i = 1, aktualna suma = 0, aktualny wyraz = 1
  }
}


calculateSum(5, 2.0)

//  --  ZADANIE 3 ------------------------------------------------------------------------------------------------------

def findNthTerm(r: Double): Int = {
  def aux(n: Int, current_sum: Double): Int = {                                                                         //  Funkcja pomocnicza
    if (current_sum > r) n                                                                                              //  Jeśli aktualna suma jest większa od zadanej wartości r, zwroc n
    else {                                                                                                              //  W przeciwnym wypadku:
      val next_term = 1.0 / n                                                                                           //  Oblicz następny wyraz ciągu ze wzoru 1 / i
      aux(n + 1, current_sum + next_term)                                                                               //  Wywołaj funkcję pomocniczą dla inkrementowanego n oraz aktualnej sumy zwiększonej o nastęony wyraz ciągu
    }
  }

  aux(1, 0.0)                                                                                                           //  Wywołaj funkcje pomocniczą dla n = 1 oraz aktualnej sumy = 0
}


findNthTerm(2.0)

//  --  ZADANIE 4 ------------------------------------------------------------------------------------------------------

def findMin(list: List[Double]): Double = {
  def aux(current_min: Double, current_list: List[Double]): Double = {                                                  //  Funkcja pomocnicza
    if (current_list == Nil) current_min                                                                                //  Jeśli aktualna lista jest pusta, zwroc aktualną najmniejszą wartośc
    else {                                                                                                              //  W przeciwnym wypadku:
      val current_element = current_list.head                                                                           //  Niech aktualnie rozpatrywany element to głowa aktualnej listy
      val current_tail = current_list.tail                                                                              //  Niech aktualny ogon listy to ogon aktualnej listy (oczywiste)

      if (current_element < current_min) aux(current_element, current_tail)                                             //  Jeśli głowa aktualnej listy jest mniejsza niz aktualna najmniejsza wartość, wywołaj funkcje pomocniczą dla ogona aktualnej listy oraz głowy aktualnej listy jako najmniejszy element
      else aux(current_min, current_tail)                                                                               //  W przeciwnym wypadku, wywołaj funkcje pomocniczą dla ogona aktualnej listy i aktualnej najmniejszej wartości
    }
  }

  aux(Int.MaxValue, list)                                                                                               //  Wywołanie funkcji pomocniczej dla zadanej listy i aktualnej najmniejszej wartości = Int.MaxValue
}


findMin(List(5.0, 1.0, -24.0, 623.0))

//  --  ZADANIE 5 ------------------------------------------------------------------------------------------------------

def divide(list: List[Int]): (List[Int], List[Int]) = {
  def aux(current_result: (List[Int], List[Int]), current_list: List[Int]): (List[Int], List[Int]) = {                  //  Funkcja pomocnicza
    if (current_list == Nil) current_result                                                                             //  Jeśli aktualna lista jest pusta, zwroc pare list wynikowych
    else {                                                                                                              //  W przeciwnym wypadku:
      val current_element = current_list.head                                                                           //  Niech aktualnie rozpatrywany element to głowa aktualnej listy
      val current_tail = current_list.tail                                                                              //  Niech aktualny ogon listy to ogon aktualnej listy (oczywiste)

      if (current_element < 0) aux((current_element :: current_result._1, current_result._2), current_tail)             //  Jeśli aktualnie rozpatrywany element jest < 0, dodaj go do listy wynikowej liczb mniejszych od 0
      else aux((current_result._1, current_element :: current_result._2), current_tail)                                 //  W przeciwnym wypadku, dodaj go do listy wynikowej liczb wiekszych lub rownych 0
    }
  }

  aux((Nil, Nil), list)                                                                                                 //  Wywołanie funkcji pomocniczej dla pary pustych list i zadanej listy jako listy aktualnej
}


divide(List(5, 1, -24, 623))
