import scala.annotation.tailrec
//  LISTA 4

//  --  ZADANIE 3 ------------------------------------------------------------------------------------------------------

//  Definicja drzewa binarnego z wykładu:

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val tt = Node(1,                                                                                                        //                               1
              Node(2,                                                                                                   //                             /   \
                   Node(4,                                                                                              //                            2      3
                        Empty,                                                                                          //                          /       /   
                        Empty),                                                                                         //                         4       5
                   Empty                                                                                                //                                   \
                  ),                                                                                                    //                                     6
              Node(3,                                                                                                    
                   Node(5,
                        Empty,
                        Node(6,
                             Empty,
                             Empty
                            )
                        ),
                   Empty
                  )
              )

//  Wersja nieoptymalna   - O(n^2)
def breadthBT[A](tree: BT[A]): List[A] = tree match {                                                                  //  Dopasowanie drzewa do wzorca
  case Empty => Nil                                                                                                    //  Jeśli drzewo jest puste, zwróć pustą listę
  case Node(value, left, right) => value :: breadthBT(left) ::: breadthBT(right)                                       //  Jeśli drzewo nie jest puste, dodaj wartość węzła do połączonej listy z wywołań funkcji dla lewego i prawego poddrzewa tego węzła (drzewa)
}

//  Wersja optymalna      - O(n)
def optBreadthBT[A](tree: BT[A]): List[A] = {                                                                          
  def helperFunc(pair: (BT[A], List[A])): List[A] = pair match {                                                        //  Funkcja pomocnicza, dopasowanie pary (Drzewo, Lista (akumulator wyniku)) do wzorca
    case (Empty, list) => list                                                                                          //  Jeśli drzewo jest puste, zwróć listę
    case (Node(value, left, right), list) => value :: helperFunc(left, helperFunc(right, list))                         //  Jeśli drzwo nie jest puste, dodaj wartość węzła do listy wynikowej wywołania funkcji dla lewego poddrzewa i listy otrzymanej z wywołania funkcji dla prawego poddrzewa
  }

  helperFunc((tree, Nil))
}

breadthBT(tt)
optBreadthBT(tt)

//  --  ZADANIE 4 ------------------------------------------------------------------------------------------------------

//  a) Długość ścieżki wewnętrznej  - O(n)

def internalPathLength[A](tree: BT[A]): Int = {
  def helperFunc(tree: BT[A], depth: Int): Int = tree match {                                                            //  Funkcja pomocnicza, dopasowanie drzewa do wzorca
    case Empty => 0                                                                                                      //  Jeśli drzewo jest puste, zwróć 0
    case Node (_, left, right) => depth + helperFunc (left, depth + 1) + helperFunc (right, depth + 1)                   //  Jeśli drzewo nie jest puste, dodaj aktualny wynik (głębokość drzewa) do wartości wywołań funkcji dla lewego i prawego poddrzewa z nową aktualną głębokością zwiększoną o 1
  }

  helperFunc(tree, 0)                                                                                                    //  Wywołanie funkcji pomocniczej dla zadnego drzewa i aktualnej głębokości = 0
}

//  b) Długość ścieżki zewnętrznej   - O(n)

def externalPathLength[A](tree: BT[A]): Int = {
  def helperFunc(tree: BT[A], depth: Int): Int = tree match {                                                            //  Dopasowanie drzewa do wzorca
    case Empty => depth                                                                                                  //  Jeśli drzewo jest puste, zwróć aktualną głębokość
    case Node (_, left, right) => helperFunc (left, depth + 1) + helperFunc (right, depth + 1)                           //  Jeśli drzewo nie jest puste, dodaj wartości wywołań funkcji dla lewego i prawego poddrzewa z aktualną głębokością zwięszkoną o 1
  }

  helperFunc(tree, 0)                                                                                                    //  Wywołanie funkcji pomocniczej dla zadnego drzewa i aktualnej głębokości = 0
}

internalPathLength(tt)
externalPathLength(tt)

//  --  ZADANIE 5 ------------------------------------------------------------------------------------------------------

sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]

//  Graf z wykładu:
val g = Graph((i: Int) =>
  i match
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => Nil
    case 4 => List(0, 2)
    case n => throw new Exception(s"Graph g: node $n doesn't exist")
)

//  O(n + m); n, m - ilość wierzchołków i krawędzi
def depthSearch[A](graph: Graph[A])(startNode: A): List[A] = {
  @tailrec
  def search(visited: List[A])(queue: List[A]): List[A] = queue match {
    case Nil => visited.reverse
    case listHead :: listTail => {
      if (visited contains listHead) search(visited)(listTail)
      else search(listHead :: visited)((graph succ listHead) ::: listTail)
    }
  }

  search(Nil)(List(startNode))
}

depthSearch(g)(4)
