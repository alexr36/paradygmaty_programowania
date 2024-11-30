//  LISTA 5

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

def lrepeat[A](k: Int)(lxs: LazyList[A]): LazyList[A] = {
  if (k <= 0) throw new RuntimeException(s"Cannot repeat an element less than 0 times: $k.")
  else {
    def aux[A](counter: Int, curr_elem: A, result: LazyList[A]): LazyList[A] = {
      if (counter == 0) lrepeat(k)(result)
      else curr_elem #:: aux(counter - 1, curr_elem, result)
    }

    lxs match {
      case LazyList() => LazyList()
      case x #:: xs => aux(k, x, xs)
    }
  }
}

lrepeat(3)(LazyList('a','b','c','d')).toList
lrepeat(3)(LazyList.from(1)).take(15).toList
lrepeat(3)(LazyList()).take(15).toList


//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

val lfib: LazyList[Int] = {
  def fib(a: Int, b: Int): LazyList[Int] = {
    a #:: fib(b, a + b)
  }

  fib(1, 1)
}

lfib.take(21).toList


//  --  ZADANIE 3 ------------------------------------------------------------------------------------------------------

//  Definicja polimorficznego leniwego drzewa binarnego
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

//  Podpunkt a):
def lBreadth[A](ltree: lBT[A]): LazyList[A] = {
  def aux(queue: List[lBT[A]]): LazyList[A] = queue match {
    case Nil => LazyList().empty
    case LEmpty :: rest => aux(rest)
    case LNode(x, left, right) :: rest =>
      x #:: aux(rest :+ left() :+ right())
  }

  aux(List(ltree))
}

//  Podpunkt b):
def lTree(n: Int): lBT[Int] = {
  LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))
}

lBreadth(lTree(1)).take(20).toList
lBreadth(LEmpty).take(20).toList