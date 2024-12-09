import scala.annotation.tailrec
//  LISTA 3

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

def repeatElements[A](list: List[A]): List[A] = {
  @tailrec
  def addRepeats(element: A, amount: Int, res: List[A]): List[A] = {                                                    //  Funkcja pomocnicza do powielania elementow
    if (amount > 0) addRepeats(element, amount - 1, element :: res)                                                     //  Jeśli ilosc powtorzen jest > 0, zmniejsz ilosc powtorzen i dodaj element do listy wynikowej, wywolaj funkcje ponownie
    else res                                                                                                            //  W przeciwnym wypadku, zwroc wynik
  }

  @tailrec
  def iterateList(pos: Int, current_list: List[A], result: List[A]): List[A] = {                                        //  Funkcja pomocnicza do przechodzenia przez listę
    current_list match {                                                                                                //  Dopasuj aktualną liste do wzorca
      case Nil => result.reverse                                                                                        //  Jeśli lista jest pusta, odwroc wynik i zwroc go
      case head :: tail =>                                                                                              //  Jeśli lista nie jest pusta,
        iterateList(pos + 1, tail, addRepeats(head, pos, Nil) ::: result)                                               //  przejdz do nastpenej pozycji, jako aktualna listę podaj ogon poprzedniej listy oraz do wyniku dodaj wynik funkcji powielającej elementy
    }
  }

  iterateList(1, list, Nil)                                                                                             //  Wywołaj funkcję do przechodzenia przez listę
}

val list = List(1, 2, 3, 4, 5, 5, 6, 7, 8, 9)
repeatElements(list)
