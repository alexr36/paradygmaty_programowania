import scala.annotation.tailrec
import math.Ordering

//  LISTA 6

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

@tailrec
def whileLoop(condition: () => Boolean, expression: () => Unit): Unit = {
  if (condition()) {                                                                                                    //  Jeśli warunek jest spełniony,
    expression()                                                                                                        //  Wykonaj kod zmieniający stan wewnętrzny programu
    whileLoop(condition, expression)                                                                                    //  Wykonaj funkjcę (pętlę) ponownie
  }
}

def test(): Unit = {
  var count = 0
  whileLoop(() => count < 3, () => { println(count); count += 1 })
}

test()

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

//  Funkcje z wykładu przepisane na język Scala:

//  a)
def swap[A](tab: Array[A], i: Int, j: Int): Unit = {
  val aux = tab(i)
  tab(i) = tab(j)
  tab(j) = aux
}


def choosePivot[A](tab: Array[A], m: Int, n: Int): A = {
  tab((m + n) / 2)
}


//  b)
def partition[A](tab: Array[A], l: Int, r: Int)(implicit ord: Ordering[A]): (Int, Int) = {
  import ord._

  var i = l
  var j = r
  val pivot = choosePivot(tab, l, r)

  while (i <= j) do {
    while (tab(i) < pivot) do i += 1
    while (pivot < tab(j)) do j -= 1

    if (i <= j) {
      swap(tab, i, j)
      i += 1
      j -= 1
    }
  }

  (i, j)
}


//  c)
def quick[A](tab: Array[A], l: Int, r: Int)(implicit ord: Ordering[A]): Unit = {
  if (l < r) {
    val (i, j) = partition(tab, l, r)

    if (j - l < r - i) {
      quick(tab, l, j)
      quick(tab, i, r)
    }
    else {
      quick(tab, i, r)
      quick(tab, l, j)
    }
  }
}


//  d)
def quicksort[A](tab: Array[A])(implicit ord: Ordering[A]): Unit = {
  quick(tab, 0, tab.length - 1)
}


val t1 = Array(4, 8, 1, 12, 7, 3, 1, 9)
quicksort(t1)
t1

val t2 = Array("kobyla", "ma", "maly", "bok")
quicksort(t2)
t2
