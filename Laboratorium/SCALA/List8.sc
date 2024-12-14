//  LISTA 8

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

sealed trait BT[+A]                                                                                                     //  Definicja typu dzrzewa
case object Empty extends BT[Nothing]
case class Node[A](value: A, left: BT[A], right: BT[A]) extends BT[A]

def toListSumTree(tree: BT[List[Int]]): BT[Int] = {
  tree match {                                                                                                          //  Dopasuj drzewo do wzorca
    case Empty => Empty                                                                                                 //  Jeśli drzewo jest puste, zwróć drzewo puste
    case Node(value, left, right) =>                                                                                    //  Jeśli drzwo nie jest puste,
      Node(value.foldLeft(0)((res, elem) => elem + res), toListSumTree(left), toListSumTree(right))                     //  Zwróć drzewo, zsumuj elementy listy oraz wywołaj funkcje dla lewego i prawego poddrzewa
  }
}


val tree = Node(List(1, 2, 3),                                                                                          //  Przykładowe drzewo
                Node(List(4, 5),
                    Empty,
                    Empty
                ),
                Node(List(6),
                    Empty,
                    Node(List(7, 8),
                        Empty,
                        Empty
                    )
                )
          )

toListSumTree(tree)


//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

sealed trait Graph[+A]
case class GraphNode[A](value: A, neighbors: List[GraphNode[A]]) extends Graph[A]                                       //  Definicja grafu

def graphToBinaryTree[A](graph_node: GraphNode[A]): BT[A] = {
  def aux(queue: List[GraphNode[A]]): BT[A] = {
    queue match {
      case Nil => Empty
      case head :: tail =>
        Node(head.value, graphToBinaryTree(head), aux(tail))
    }
  }

  Node(graph_node.value, aux(graph_node.neighbors), Empty)
}


val nodeF = GraphNode("F", List())
val nodeD = GraphNode("D", List())
val nodeB = GraphNode("B", List(nodeD))
val nodeC = GraphNode("C", List())
val nodeA = GraphNode("A", List(nodeB, nodeC, nodeF))
val nodeE = GraphNode("E", List(nodeA))


graphToBinaryTree(nodeE)
