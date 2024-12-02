import scala.annotation.tailrec
//  LISTA 1

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

def flatten1[A](xss: List[List[A]]): List[A] = {
  if (xss.isEmpty) throw new RuntimeException(s"Podana lista jest pusta: $xss.")                                        //  Jeśli lista list jest pusta, rzuć wyjątek
  if (xss.tail.isEmpty) xss.head                                                                                        //  Jeśli ogon listy list jest pusty, zwróć głowę tej listy
  else xss.head ::: flatten1(xss.tail)                                                                                  //  Jeśli ogon listy nie jest pusty, dodaj listę będącą głową listy list do listy wynikowej wywołania funkcji dla ogona listy list
}

//  TESTY
flatten1(List(List(5, 6), List(1, 2, 3)))
flatten1(List(List(5, 6), List(1, 2, 3), List(7, 8, 9)))
flatten1(List(List(5, 6)))
flatten1(List())

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

def count[A](x: A, xs: List[A]): Int = {
  if (xs.isEmpty) 0                                                                                                      //  Jeśli lista jest pusta, to zwróć 0
  else if (xs.head == x) 1 + count(x, xs.tail)                                                                           //  Jeśli głowa listy jest równa wartości, której liczbę powtórzeń liczymy, dodaj 1 do wywołania funkcji dla ogona listy
  else count(x, xs.tail)                                                                                                 //  Jeśli głowa listy nie jest równa tej wartości, wowołaj funkcję dla ogona listy
}

//  TESTY
count('a', List('a', 'l', 'a'))
count(5, List(5, 5, 1, 1, 2, 5, 6))
count("Default", List("Aa", "Bb", "Cc"))

//  --  ZADANIE 3 ------------------------------------------------------------------------------------------------------

def replicate[A](x: A, n: Int): List[A] = {
  if (n < 0) throw new RuntimeException(s"Element nie moze byc powtorzony ujemna ilosc razy: $n.")                      //  Jeśli n jest mniejsze od 0, rzuć wyjątek        
  else if (n == 0) Nil                                                                                                  //  Jeśli n jest równe 0, to zwróć listę pustą
  else x :: replicate(x, n - 1)                                                                                         //  Jeśli n jest większe od 0, dodaj powtarzany element do wyniku wywołania funkcji dla (n - 1
}

//  TEST
replicate("la", 3)
replicate(5, 15)
replicate('s', 0)
replicate(1000, -4)

//  --  ZADANIE 4 ------------------------------------------------------------------------------------------------------

//  Jako metoda
def sqrListMet(xs: List[Int]): List[Int] = {
  if (xs.isEmpty) Nil                                                                                                    //  Jeśli lista jest pusta, zwróć listę pustą
  else xs.head * xs.head :: sqrListMet(xs.tail)                                                                          //  W przeciwnym wypadku, dodaj kwadrat głowy listy do listy wynikowej wywołania funkcji dla ogona listy
}

sqrListMet(List(1, 2, 3, -4))
sqrListMet(List(6, 4, 9, 15))
sqrListMet(List())

//  Jako funkcja anonimowa
val sqrList: List[Int] => List[Int] = xs => {                                                                            //  Analogicznie jak wyżej
  if (xs.isEmpty) Nil
  else xs.head * xs.head :: sqrList(xs.tail)
}

sqrList(List(1, 2, 3, -4))
sqrList(List(6, 4, 9, 15))
sqrList(List())

//  --  ZADANIE 5 ------------------------------------------------------------------------------------------------------

def palindromel[A](xs: List[A]): Boolean = {
  xs == xs.reverse                                                                                                       //  Sprawdzenie, czy odwrócona lista jest równa samej sobie
}

//  TESTY

palindrome(List('a', 'l', 'a'))
palindrome(List(1, 2, 3, 4, 5))
palindrome(List("Jestem", "Palindromem", "Jestem"))
palindrome(List(585))
palindrome(List())

//  --  ZADANIE 6 ------------------------------------------------------------------------------------------------------

def listLength[A](xs: List[A]): Int = {  
  if (xs.isEmpty) 0                                                                                                      //  Jeśli lista jest pusta, zwróć 0
  else 1 + listLength(xs.tail)                                                                                           //  W przeciwnym wypadku, dodaj 1 do wyniku wywołania funkcji dla ogona listy
}

//  TESTY
listLength(List(1, 2, 3, 4, 5))
listLength(List("Xx", "Yy", "Zz"))
listLength(List())
