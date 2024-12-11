//  LISTA 6

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

type Samochod = (String, String, Int)                                                                                   //  Definicja typu Samochod
type Samochody = List[Samochod]                                                                                         //  Definicja typu Samochody

val cars: Samochody = List(                                                                                             //  Przykładowa lista samochodów z zadania
  ("Opel", "astra", 1999),
  ("Renault", "megane", 2004),
  ("Opel", "corsa", 2009),
  ("Nissan", "micra", 2004),
  ("Opel", "corsa", 2009),
  ("Nissan", "micra", 2003)
)


def countBrands(car_list: Samochody): List[(String, Int)] = {
  car_list.foldLeft(Map.empty[String, Int])((result, car) => {                                                          //  Użyj funkcjonału foldLeft aby przeiterować po liście, result będzie słownikiem Map
    val brand = car._1                                                                                                  //  Niech 'brand' będzie przechowywać informacje o marce aktualnego samochodu

    result + (brand -> result.get(brand).map(_ + 1).getOrElse(1))                                                       //  Niech w wynikowej mapie, jeśli klucz 'brand' już występuje, wartość odpowiadająca mu zostanie zinkrementowana, jeśli nie, niech 'brand' zostanie dodany z wartością 1
  }).toList                                                                                                             //  Konwersja Map na List
}

countBrands(cars)

//  --  ZADANIE 3 ------------------------------------------------------------------------------------------------------

sealed trait PS[+A, +B]                                                                                                 //  Definicja elementu - listy heterogenicznej
case class P[A, B](e: A) extends PS[A, B]
case class S[A, B](e: B) extends PS[A, B]

sealed trait Tree[+A]                                                                                                   //  Definicja drzewa
case class Leaf[A](value: A) extends Tree[A]
case class SingleNode[A](child: Tree[A]) extends Tree[A]
case class DoubleNode[A](left: Tree[A], right: Tree[A]) extends Tree[A]

val dana = "Dana"
val wezel = "Wezel"
val lewo = "lewo"
val prawo = "prawo"
val element = "element"

val tree: Tree[Int] = DoubleNode(                                                                                       //  Drzewo z przykładu
                                DoubleNode(
                                          DoubleNode(
                                                    Leaf(2),
                                                    Leaf(1)
                                          ),
                                          SingleNode(
                                                    Leaf(4)
                                          )
                                ),
                                Leaf(3)
                      )


def toListPostfix[A](tree: Tree[A]): List[PS[(String, A), String]] = {
  tree match {                                                                                                          //  Dopasuj drzewo do wzorca
    case Leaf(value) =>                                                                                                 //  Jeśli drzewo to liść,
      List(P(("Dana", value)))                                                                                          //  zwróć P((Dana, value)

    case SingleNode(child) => {                                                                                         //  Jeśli drzewo to pojedynczy węzeł,
      val child_list = toListPostfix(child)                                                                             //  Niech child_list to lista wynikowa drzew (węzłów) dla jedynego potomka
      child_list ::: List(S("Wezel, (element"))                                                                         //  Dodaj listę dla potomków do jednoelementowej listy wynikowej dla pierwotnego drzewa
    }

    case DoubleNode(left, right) => {                                                                                   //  Jeśli drzewo to podwójny węzeł,
      val left_list = toListPostfix(left)                                                                               //  Niech left_list to lista wynikowa drzew (węzłów) dla lewego potomka
      val right_list = toListPostfix(right)                                                                             //  Niech right_list to lista wynikowa drzew (węzłów) dla prawego potomka
      val description = {                                                                                               //  Niech zmienna description (opis) równa jest:
        if (left.isInstanceOf[Leaf[_]] && right.isInstanceOf[Leaf[_]]) {                                                //  Jeśli oba potomki są liśćmi:
          "Wezel, (element, element)"                                                                                   //  S(Wezel, (element, element))
        }
        else if (left.isInstanceOf[Leaf[_]]) {                                                                          //  Jeśli lewy potomek jest liściem:
          "Wezel, (element, prawo)"                                                                                     //  S(Wezel, (element, prawo))
        }
        else if (right.isInstanceOf[Leaf[_]]) {                                                                         //  Jeśli prawy potomek jest liściem:
          "Wezel (lewo, element)"                                                                                       //  S(Wezel, (lewo, element))
        }
        else {                                                                                                          //  Jeśli żaden potomek nie jest liściem:
          "Wezel (lewo, prawo)"                                                                                         // S(Wezel, (lewo, prawo))
        }
      }

      left_list ::: right_list ::: List(S(description))                                                                 //  Połącz ze sobą listy wynikowe w kolejności odwrotnej do tej, w której zostały otrzymane
    }
  }
}


toListPostfix(tree)
