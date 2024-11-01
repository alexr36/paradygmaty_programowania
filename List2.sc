import scala.annotation.tailrec

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

//  Wersja z definicji
def fib(n: Int): Int = n match {
  case n if (n < 0) => throw new RuntimeException(s"Nie mozna obliczyc wyrazu ciagu o pozycji mniejszej niz zero: $n.")
  case 0 => 0
  case 1 => 1
  case n => fib(n - 1) + fib (n - 2)
}

//  Wersja z rekurenjcą ogonową
def fibTail(n: Int): Int = {
  @tailrec
  def helperFunc(n: Int, previousTerm: Int, currentTerm: Int): Int = n match {
    case n if (n < 0) => throw new RuntimeException(s"Nie mozna obliczyc wyrazu ciagu o pozycji mniejszej niz zero: $n.")
    case 0 => previousTerm
    case n => helperFunc(n - 1, currentTerm, currentTerm + previousTerm)
  }

  helperFunc(n, 0, 1)
}

fib(42)                     //  - złożoność: O(2^n)
fibTail(42)                 //  - złożoność: O(n)

//  --  ZADANIE 3 ------------------------------------------------------------------------------------------------------

//  Jako metoda
def root3Met(a: Double): Double = {
  val epsilon = 1e-15

  @tailrec
  def helperFunc(x: Double): Double = {
    if (Math.abs(Math.pow(x, 3) - a) <= epsilon * Math.abs(a)) x
    else {
      val x_next = x + (a / Math.pow(x, 2) - x) / 3
      helperFunc(x_next)
    }
  }

  val x0 = {
    if (a > 1) a / 3
    else a
  }

  helperFunc(x0)
}

//  Jako funkcja
def root3Fun: (Double => Double) = a => {
  val epsilon = 1e-15

  @tailrec
  def helperFunc(x: Double): Double = {
    if (Math.abs(Math.pow(x, 3) - a) <= epsilon * Math.abs(a)) x
    else {
      val x_next = x + (a / Math.pow(x, 2) - x) / 3
      helperFunc(x_next)
    }
  }

  val x0 = {
    if (a > 1) a / 3
    else a
  }

  helperFunc(x0)
}

root3Met(27)                  //  - złożoność: O(log(1/epsilon))
root3Met(8)
root3Fun(27)                  //  - złożoność: O(log(1/epsilon))
root3Fun(8)

//  --  ZADANIE 4 ------------------------------------------------------------------------------------------------------

//  Podpunkt a)
def matchCaseOne[A](list: List[A]): String = list match {
  case List(_, _, x, _, _) if (x == 0) => "Dopasowano wzorzec z x = 0."
  case _ => "Nie dopasowano wzorca."
}

//  Podpunkt b)
def matchCaseTwo[A](list: List[(A, A)]): String = list match {
  case List((_, _), (x, _)) if (x == 0) => "Dopasowano wzorzez z x = 0."
  case _ => "Nie dopasowano wzorca."
}

matchCaseOne(List(-2, -1, 0, 1, 2))       //  - złożoność: O(1)
matchCaseOne(List(1, 2, 3, 4))
matchCaseTwo(List((1, 2), (0, 1)))        //  - złożoność: O(1)
matchCaseTwo(List((1, 3), (4, 0)))

//  --  ZADANIE 5 ------------------------------------------------------------------------------------------------------

@tailrec
def initSegment[A](xs: List[A], ys: List[A]): Boolean = (xs, ys) match {
  case (Nil, _) => true
  case (_, Nil) => false
  case (xs_head :: xs_tail, ys_head :: ys_tail) => (xs_head == ys_head) && initSegment(xs_tail, ys_tail)
}

initSegment(List(1, 2), List(1, 2, 3, 4))       //  - złożoność: O(min(m, n)); m, n - długości list
initSegment(List(1, 2), List(1, 3, 2, 4))

//  --  ZADANIE 6 ------------------------------------------------------------------------------------------------------

def replaceNth[A](xs: List[A], n: Int, x: A): List[A] = xs match {
  case Nil => Nil
  case _ :: listTail if (n == 0) => x :: listTail
  case listHead :: listTail => listHead :: replaceNth(listTail, n - 1, x)
}

replaceNth(List('o', 'l', 'a', 'm', 'a', 'k', 'o', 't', 'a'), 1, 's')      //  - złożoność: O(n)
replaceNth(List('o', 'l', 'a', 'm', 'a', 'k', 'o', 't', 'a'), -4, 's')