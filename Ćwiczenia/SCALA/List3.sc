//  LISTA 3

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

//  a)

def uncurry3[A, B, C, D](f: (A, B, C) => D): A => B => C => D = {
  case (x: A) => (y: B) => (z: C) => f(x, y, z)
}

/*

  Nie jest określone jaki typ mają przujmować funkcje, a zatem:
  f: (A, B, C) => D
  uncurry3: ((A, B, C) => D) => A => B => C => D

 */

//  b)

def curry3[A, B, C, D](f: A => B => C => D): (A, B, C) => D = {
  case (x, y, z) => f(x)(y)(z)
}

/*

  Nie jest określone jaki typ mają przujmować funkcje, a zatem:
  f: A => B => C => D  
  curry3: (A => B => C => D) => (A, B, C) => D

 */

//  --  ZADANIE 3 ------------------------------------------------------------------------------------------------------

def sumProd(xs: List[Int]): (Int, Int) = {
  xs.foldLeft((0, 1)) { case ((sum, product), x) => (sum + x, product * x) }

}

//  --  ZADANIE 5 ------------------------------------------------------------------------------------------------------

//  Funkcja do porównywania
def compare(x: Int)(y: Int): Boolean = x <= y

//  a)

//  Funkcja pomocnicza do wstawiania elementów na odpowiednie miejsce
def insert(compFunc: Int => Int => Boolean, x: Int, list: List[Int]): List[Int] = list match {                            //  Dopasowanie listy do wzorca
  case Nil => List(x)                                                                                                     //  Jeśli lista jest pusta, zwróć jednoelementową listę zawierającą wstawiany element
  case listHead :: listTail => {                                                                                          //  Jeśli lista nie jest pusta, weź jej głowę
    if compFunc(x, listHead) then x :: list                                                                               //  Jeśli wstawiany element jest mniejszy lub równy głowie listy, dodaj element do listy
    else listHead :: insert(compFunc, x, listTail)                                                                        //  W przeciwnym wypadku dodaj głowę listy do listy wynikowej wywołania funkcji dla wstawianego elementu i ogona pierwotnej listy
  }
}

def insertionsort(compFunc: Int => Int => Boolean, list: List[Int]): List[Int] = list match {                             //  Dopasowanie listy do wzorca
  case Nil => Nil                                                                                                         //  Jeśli lista jest pusta, zwróć listę pustą
  case listHead :: listTail => insert(compFunc, listHead, insertionsort(compFunc, listTail))                              //  Jeśli lista nie jest pusta, weź jej głowę i umieść na odpowiednim miejscu w liście otrzymanej z wywołania funkcji dla ogona pierwotnej listy
}

insertionsort(compare, List(6, 7, 21, 1, 8, 24, 51, 2, 2))

//  b)

//  Funkcja pomocnicza do scalania list
def merge(compFunc: Int => Int => Boolean, listA: List[Int], listB: List[Int]): List[Int] = (listA, listB) match {        //  Dopasowanie pary list do wzorca
  case (_, Nil) => listA                                                                                                  //  Jeśli prawa lista jest pusta, zwróć lewą
  case (Nil, _) => listB                                                                                                  //  Jeśli lewa lista jest pusta, zwróć prawą
  case (listHeadA :: listTailA, listHeadB :: listTailB) => {                                                              //  Jeśli listy nie są puste, weź ich głowy
    if compFunc(listHeadA)(listHeadB) then listHeadA :: merge(compFunc, listTailA, listB)                                 //  Jeśli głowa listy pierwszej jest <= głowie listy drugiej, dodaj głowę listy pierwszej do listy wynikowej wywołania funkcji dla ogona listy pierwszej i pierwotnej listy drugiej
    else listHeadB :: merge(compFunc, listTailB, listA)                                                                   //  W przeciwnym wypadku, odwrotnie
  }
}

//  Funkcja pomocnicza do dzielenia list
def divide(list: List[Int]): (List[Int], List[Int]) = list match {                                                        //  Dopasowanie listy do wzorca
  case Nil => (Nil, Nil)                                                                                                  //  Jeśli lista jest pusta, zwróć parę list pustych
  case List(x) => (List(x), Nil)                                                                                          //  Jeśli lista jest jednoelementowa, niech będzie pierwszą listą w parze
  case listHead1 :: listHead2 :: listTail => {                                                                            //  Jeśli lista nie jest pusta, weź dwa jej pierwsze elementy
    val (leftList, rightList) = divide(listTail)                                                                          //  Niech para list (leftList, rightList) będzie wynikiem podziału listy z wywołania funkcji dla ogona pierwotnej listy
    (listHead1 :: leftList, listHead2 :: rightList)                                                                       //  Dodaj pierwszy element do leftList i drugi do rightList
  }
}

def mergesort(compFunc: Int => Int => Boolean, list: List[Int]): List[Int] = list match {                                 //  Dopasowanie listy do wzorca
  case Nil => Nil                                                                                                         //  Jeśli lista jest pusta, zwróć listę pustą
  case List(_) => list                                                                                                    //  Jeśli lista jest jednoelementowa, zwróć tę listę
  case _ => {                                                                                                             //  Jeśli lista ma więcej niż 1 element:
    val (leftList, rightList) = divide(list)                                                                              //  - Niech para list (leftList, rightList) będzie wynikiem podziału listy z wywołania funkcji dzielenia list
    merge(compFunc, mergesort(compFunc, leftList), mergesort(compFunc, rightList))                                        //  - Wywołanie funkcji łączenia list dla list wynikowych wywołań funkcji sortowania dla leftList i rightList
  }
}

mergesort(compare, List(6, 7, 21, 1, 8, 24, 51, 2, 2))
