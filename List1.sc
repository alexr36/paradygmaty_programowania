import scala.annotation.tailrec

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

def flatten1[A](xss: List[List[A]]): List[A] = {
  if (xss.isEmpty) throw new RuntimeException(s"Podana lista jest pusta: $xss.")
  else if (xss.tail.isEmpty) xss.head
  else xss.head ::: flatten1(xss.tail)
}

//  TESTY
flatten1(List(List(5, 6), List(1, 2, 3)))
flatten1(List(List(5, 6), List(1, 2, 3), List(7, 8, 9)))
flatten1(List(List(5, 6)))
flatten1(List())

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

def count[A](x: A, xs: List[A]): Int = {
  if (xs.isEmpty) 0
  else if (xs.head == x) 1 + count(x, xs.tail)
  else count(x, xs.tail)
}

//  TESTY
count('a', List('a', 'l', 'a'))
count(5, List(5, 5, 1, 1, 2, 5, 6))
count("Default", List("Aa", "Bb", "Cc"))

//  --  ZADANIE 3 ------------------------------------------------------------------------------------------------------

def replicate[A](x: A, n: Int): List[A] = {
  if (n < 0) throw new RuntimeException(s"Element nie moze byc powtorzony ujemna ilosc razy: $n.")
  else if (n == 0) Nil
  else x :: replicate(x, n - 1)
}

//  TEST
replicate("la", 3)
replicate(5, 15)
replicate('s', 0)
replicate(1000, -4)

//  --  ZADANIE 4 ------------------------------------------------------------------------------------------------------

//  Jako metoda
def sqrListMet(xs: List[Int]): List[Int] = {
  if (xs.isEmpty) Nil
  else xs.head * xs.head :: sqrListMet(xs.tail)
}

sqrListMet(List(1, 2, 3, -4))
sqrListMet(List(6, 4, 9, 15))
sqrListMet(List())

//  Jako funkcja anonimowa
val sqrList: List[Int] => List[Int] = xs => {
  if (xs.isEmpty) Nil
  else xs.head * xs.head :: sqrList(xs.tail)
}

sqrList(List(1, 2, 3, -4))
sqrList(List(6, 4, 9, 15))
sqrList(List())

//  --  ZADANIE 5 ------------------------------------------------------------------------------------------------------

//  Osobiście zdefiniowana funkcja reverseList
def reverseList[A](xs: List[A]): List[A] = {
  if (xs.isEmpty) Nil
  else xs.last :: reverseList(xs.init)
}

//  Z wbudowaną funkcją .reverse
def palindromeBuiltInRev[A](xs: List[A]): Boolean = {
  if (xs.isEmpty || xs.tail.isEmpty) true
  else xs == xs.reverse
}

//  Z osobiście zdefiniowaną funkcją reverseList
def palindromeCustomRev[A](xs: List[A]): Boolean = {
  if (xs.isEmpty || xs.tail.isEmpty) true
  else xs == reverseList(xs)
}

//  Bez wykorzystania .reverse ani reverseList
@tailrec
def palindrome[A](xs: List[A]): Boolean = {
  if (xs.isEmpty || xs.tail.isEmpty) true
  else if (xs.head == xs.last) palindrome(xs.tail.init)
  else false
}

def palindromeExperimental[A](xs: List[A]): Boolean = {
  xs == xs.reverse
}

//  TESTY
reverseList(List('a', 'l', 'a'))
reverseList(List(1, 2, 3, 4, 5))
reverseList(List("Jestem", "Palindromem", "Jestem"))
reverseList(List(585))
reverseList(List())

palindromeCustomRev(List('a', 'l', 'a'))
palindromeCustomRev(List(1, 2, 3, 4, 5))
palindromeCustomRev(List("Jestem", "Palindromem", "Jestem"))
palindromeCustomRev(List(585))
palindromeCustomRev(List())

palindromeBuiltInRev(List('a', 'l', 'a'))
palindromeBuiltInRev(List(1, 2, 3, 4, 5))
palindromeBuiltInRev(List("Jestem", "Palindromem", "Jestem"))
palindromeBuiltInRev(List(585))
palindromeBuiltInRev(List())

palindrome(List('a', 'l', 'a'))
palindrome(List(1, 2, 3, 4, 5))
palindrome(List("Jestem", "Palindromem", "Jestem"))
palindrome(List(585))
palindrome(List())

palindromeExperimental(List('a', 'l', 'a'))
palindromeExperimental(List(1, 2, 3, 4, 5))
palindromeExperimental(List("Jestem", "Palindromem", "Jestem"))
palindromeExperimental(List(585))
palindromeExperimental(List())

//  --  ZADANIE 6 ------------------------------------------------------------------------------------------------------

def listLength[A](xs: List[A]): Int = {
  if (xs.isEmpty) 0
  else 1 + listLength(xs.tail)
}

//  TESTY
listLength(List(1, 2, 3, 4, 5))
listLength(List("Xx", "Yy", "Zz"))
listLength(List())