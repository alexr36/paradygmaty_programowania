import scala.annotation.tailrec
//  LISTA 2

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

def isNonIncreasing(list: List[Int]): Boolean = {
  if (list.isEmpty) true                                                                                                //  Jeśli lista jest pusta, przyjmij, że jest odpowiednio posortowana
  else {                                                                                                                //  W przciwnym wypadku:
    @tailrec
    def aux(passed_list: List[Int]): Boolean = {                                                                        //  Funkcja pomocnicza
      if (passed_list.length == 1) true                                                                                 //  Jeśli długość listy jest = 1, przyjmij, że lista jest posortowana
      else (passed_list.head >= passed_list.tail.head) && aux(passed_list.tail)                                         //  W przeciwnym wypadku sprawdzaj kolejne elementy
    }

    aux(list)                                                                                                           //  Wywołanie funkcji pomocniczej
  }
}


isNonIncreasing(List(8, 3))
isNonIncreasing(List(1, 2, 3, 4, 5, 6))
isNonIncreasing(List())
isNonIncreasing(List(2))