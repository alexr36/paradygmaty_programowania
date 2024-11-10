//  LISTA 3

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

//  a)

def curry3[A, B, C, D](f: A => B => C => D): (A, B, C) => D = {
  case (x, y, z) => f(x)(y)(z)
}

/*

  Nie jest określone jaki typ mają przujmować funkcje, a zatem:
  f:  A => B => C => D
  curry3: (A => B => C => D) => (A, B, C) => D

 */

//  b)

def uncurry3[A, B, C, D](f: (A, B, C) => D): A => B => C => D = {
  case (x: A) => (y: B) => (z: C) => f(x, y, z)
}

/*

  Nie jest określone jaki typ mają przujmować funkcje, a zatem:
  f:  (A, B, C) => D
  curry3: ((A, B, C) => D) => A => B => C => D

 */

//  --  ZADANIE 3 ------------------------------------------------------------------------------------------------------

def sumProd(xs: List[Int]): (Int, Int) = {
  xs.foldLeft((0, 1)) { case ((sum, product), x) => (sum + x, product * x) }

}

//  --  ZADANIE 5 ------------------------------------------------------------------------------------------------------

//  Funkcja do porównywania
def compare(x: Int, y: Int): Boolean = x <= y

//  a)

//  Funkcja pomocnicza do wstawiania elementów na odpowiednie miejsce
def insert(compFunc: (Int, Int) => Boolean, x: Int, list: List[Int]): List[Int] = list match {
  case Nil => List(x)
  case listHead :: listTail => {
    if compFunc(x, listHead) then x :: list
    else listHead :: insert(compFunc, x, listTail)
  }
}

def insertionsort(compFunc: (Int, Int) => Boolean, list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case listHead :: listTail => insert(compFunc, listHead, insertionsort(compFunc, listTail))
}

insertionsort(compare, List(6, 7, 21, 1, 8, 24, 51, 2, 2))

//  b)

//  Funkcja pomocnicza do scalania list
def merge(compFunc: (Int, Int) => Boolean, listA: List[Int], listB: List[Int]): List[Int] = (listA, listB) match {
  case (_, Nil) => listA
  case (Nil, _) => listB
  case (listHeadA :: listTailA, listHeadB :: listTailB) => {
    if compFunc(listHeadA, listHeadB) then listHeadA :: merge(compFunc, listTailA, listB)
    else listHeadB :: merge(compFunc, listTailB, listA)
  }
}

//  Funkcja pomocnicza do dzielenia list
def divide(list: List[Int]): (List[Int], List[Int]) = list match {
  case Nil => (Nil, Nil)
  case List(x) => (List(x), Nil)
  case listHead1 :: listHead2 :: listTail => {
    val (leftList, rightList) = divide(listTail)
    (listHead1 :: leftList, listHead2 :: rightList)
  }
}

def mergesort(compFunc: (Int, Int) => Boolean, list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case List(_) => list
  case _ => {
    val (leftList, rightList) = divide(list)
    merge(compFunc, mergesort(compFunc, leftList), mergesort(compFunc, rightList))
  }
}

mergesort(compare, List(6, 7, 21, 1, 8, 24, 51, 2, 2))
