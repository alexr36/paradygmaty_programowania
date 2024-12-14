import scala.annotation.tailrec
//  LISTA 8

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

def transformList[A](llist: LazyList[A], func: (Int => Int)): LazyList[A] = {
  def repeatElements(elem: A, count: Int): LazyList[A] =                                                                //  Funkcja pomocnicza do tworzenia podlisty leniwej z powtorzeniami zadanego elementu
    LazyList.fill(count)(elem)                                                                                          //  Zastosowanie funkcjonału fill dla liczby elementów = count i elementu wypełnienia = elem

  def aux(current_llist: LazyList[A], i: Int): LazyList[A] = {                                                          //  Funkcja pomocnicza głowna
    current_llist match {                                                                                               //  Dopasuj aktualną listę do wzorca
      case LazyList() => LazyList.empty                                                                                 //  Jeśli lista jest pusta, zwroc listę pustą
      case head #:: lazy_tail =>                                                                                        //  W przeciwnym wypadku,
        val repeats = repeatElements(head, func(i))                                                                     //  Niech repeats będzię listą leniwą złozoną z powtorzen zadanego elementu x w liczbie func(i), gdzie i to pozycja w liście wejściowej
        repeats #::: aux(lazy_tail, i + 1)                                                                              //  Dodaj do siebie listę wynikową repeats i wynikową z wywołania funkcji dla następnego elementu listy
    }
  }

  aux(llist, 1)                                                                                                         //  Wywołanie funkcji pomoczniej
}


def example_func(x: Int) = x - 1
val example_llist = LazyList.from(1)
val result = transformList(example_llist, example_func)

result.take(15).toList