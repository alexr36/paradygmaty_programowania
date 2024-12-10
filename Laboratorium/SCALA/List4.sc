import scala.annotation.tailrec
//  LISTA 4

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

def evaluateNthRoot(a: Double, n: Int, precision: Double): Double = {
  if (n <= 0 || precision <= 0)                                                                                         //  Jeśli stopień pierwiastka lub dokładność przybliżenia jest <= 0,
    throw new RuntimeException(s"Wprowadzono nieprawidlowe dane: n = $n lub precision = $precision.")                   //  rzuć wyjątek
  else {                                                                                                                //  W przeciwnym wypadku,
    @tailrec
    def aux(low_a: Double, high_b: Double, previous_eval: Double): Double = {                                           //  Funkcja pomocnicza
      val mid_c = (low_a + high_b) / 2                                                                                  //  Niech mid_c jest punktem w połowie odcinka (low_a, high_b)
      val current_eval = math.pow(mid_c, n)                                                                             //  Oblicz wartość funkcji ze wzoru a = x^n

      if (math.abs(current_eval - previous_eval) < precision) mid_c                                                     //  Jeśli spełniony jest warunek określony w zadaniu, zwróć wynik
      else if (current_eval < a) aux(mid_c, high_b, current_eval)                                                       //  Jeśli aktualne oszacowanie jest mniejsze niż pierwotna wartość a, wywołaj funkjcę pomocniczą dla odcinka (mid_c, high_b)
      else aux(low_a, mid_c, current_eval)                                                                              //  W przeciwnym wypadku, wywołaj funkcję pomocniczą dla odcnika (low_a, mid_c)
    }

    if (a < 1) aux(a, 1, 0)                                                                                             //  Jeśli a < 1, weź początkowy odcinek (a, 1)
    else aux(0, a, 0)                                                                                                   //  W przeciwnym wypadku, weź początkowy odcinek (0, a)
  }
}


evaluateNthRoot(8, 3, 0.000000001)


//  --  ZADANIE 3 ------------------------------------------------------------------------------------------------------

def trimList[A](list: List[A], amount: Int): List[A] = {
  if (amount < 0) throw new RuntimeException(s"Nie mozna usunac mniej niz 0 elementow: $amount.")                       //  Jeśli zadana liczba elementó∑ do usunięcia jest < 0, rzuć wyjątek
  else {                                                                                                                //  W przeciwnym wypadku,
    @tailrec
    def aux(current_list: List[A], remaining_amount: Int): List[A] = current_list match {                               //  Funkcja pomocnicza do usuwania elementów, dopasuj listę do wzorca
      case Nil => current_list                                                                                          //  Jeśli lista jest pusta, zwróć tę listę
      case _ :: tail if (remaining_amount == 0) => current_list                                                         //  Jeśli lista nie jest pusta a pozostała liczba elementów do usunięcia == 0, zwróć wynik
      case _ :: tail => aux(tail, remaining_amount - 1)                                                                 //  Jeśli lista nie jest pusta, ale elementów do usunięcia jest więcej, usuń element i wywołaj funkcję ponownie dla reszty listy
    }

    aux(aux(list, amount).reverse, amount).reverse                                                                      //  Wywołanie funkcji pomoczniczej
  }
}

val list = List(1, 2, 3, 4, 5, 5, 6, 7, 8, 9)
trimList(list, 2)
trimList(list, 0)
trimList(list, -1)