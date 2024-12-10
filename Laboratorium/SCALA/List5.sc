import scala.annotation.tailrec
//  LISTA 5

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

//  Wersja z rekurencją ogonową
def findMinAndMaxRec(list: List[List[Int]]): List[(Int, Int)] = {
  @tailrec
  def findMin(curr_list: List[Int], current_min: Int): Int = {                                                          //  Funkcja pomocnicza do znajdowania minimalnej wartości w liście
    curr_list match {                                                                                                   //  Dopasuj listę do wzorca
      case Nil => current_min                                                                                           //  Jeśli lista jest pusta, zwróć wynik
      case head :: tail =>                                                                                              //  Jeśli lista nie jest pusta,
        val new_min = {                                                                                                 //  Niech nowa minimalna wartość:
          if (head < current_min) head                                                                                  //  Jeśli głowa listy jest mniejsza od aktualnie najmniejszego, zwróć głowę listy
          else current_min                                                                                              //  W przeciwnym wypadku, zwróć dotychczasowo najmniejszy
        }

        findMin(tail, new_min)                                                                                          //  Wywołanie funkcji pomocniczej
    }
  }

  @tailrec
  def findMax(curr_list: List[Int], current_max: Int): Int = {                                                          //  Funkcja pomocnicza do znajdowania maksymalnej wartości w liście
    curr_list match {                                                                                                   //  Dopasuj listę do wzorca
      case Nil => current_max                                                                                           //  Jeśli lista jest pusta, zwróć wynik
      case head :: tail =>                                                                                              //  Jeśli lista nie jest pusta,
        val new_max = {                                                                                                 //  Niech nowa maksymalna wartość:
          if (head > current_max) head                                                                                  //  Jeśli głowa listy jest większa od aktualnie największego, zwróć głowę listy
          else current_max                                                                                              //  W przeciwnym wypadku, zwróć dotychczasowo największy
        }

        findMax(tail, new_max)                                                                                          //  Wywołanie funkcji pomocniczej
    }
  }

  @tailrec
  def aux(current_list: List[List[Int]], result: List[(Int, Int)]): List[(Int, Int)] = {                                //  Funkcja pomocnicza
    current_list match {                                                                                                //  Dopasuj listę do wzorca
      case Nil => result.reverse                                                                                        //  Jeśli aktualna lista jest pusta, zwróć wynik
      case head :: tail =>                                                                                              //  W przeciwnym wypadku,
        aux(tail, (findMin(head, Int.MaxValue), findMax(head, Int.MinValue)) :: result)                                 //  Wywołaj funkcję dla ogona aktualnej listy, znajdź i dodaj najmniejszy i największy element w danej liście do listy wynikowej
    }
  }

  aux(list, List())
}


//  Wersja z funkcjonałami
def findMinAndMaxFunc(list: List[List[Int]]): List[(Int, Int)] = {
  list.map(sublist =>                                                                                                   //  Zastosuj funkcjonał map() dla listy
    sublist.foldLeft((Int.MaxValue, Int.MinValue))((result, elem) =>                                                    //  Dla każdej podlisty zastosuj funkcjonał foldLeft()
      (math.min(elem, result._1), math.max(elem, result._2))))                                                          //  Znajdź i dodaj najmniejszą i największą wartość w każdej podliście do wyniku
}

val list1 = List(1, 2, 3, 4, 5, 5, 6, 7, 8, 9)
val list2 = List(420, 351, 12)
val list3 = List(53)
val list4 = List()

val list5 = List(list1, list2, list3, list4)

findMinAndMaxRec(list5)
findMinAndMaxFunc(list5)
