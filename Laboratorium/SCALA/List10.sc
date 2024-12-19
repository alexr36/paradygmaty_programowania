import scala.annotation.tailrec
//  LISTA 10

//  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------

//  Implementacja w stylu funkcyjnym
def findMinFunc(matrix: Array[Array[Int]]): Array[Int] = {
  val rows = matrix.length                                                                                              //  Liczba wierszy w macierzy
  val cols = matrix(0).length                                                                                           //  Liczba kolumn w macierzy

  def findMinInCol(col_index: Int): Int = {
    @tailrec
    def aux(row_index: Int, curren_min: Int): Int = {
      if (row_index >= rows) curren_min                                                                                 //  Jeśli indeks wiersza jest >= od liczby wierszy, zwroc aktualny wynik
      else aux(row_index + 1, math.min(curren_min, matrix(row_index)(col_index)))                                       //  W przeciwnym wypadku, wywołaj funkcję dla kolejnego wiersza
    }

    aux(0, Int.MaxValue)
  }

  Array.tabulate(cols)(findMinInCol)                                                                                    //  Zwroc wynikową tablicę o długości rownej ilości kolumn zadanej macierzy
}


//  Implementacja w stylu imperatywnym
def findMinImper(matrix: Array[Array[Int]]): Array[Int] = {
  val rows = matrix.length                                                                                              //  Liczba wierszy w macierzy
  val cols = matrix(0).length                                                                                           //  Liczba kolumn w macierzy

  val result = new Array[Int](cols)                                                                                     //  Inicjalizacja wynikowej tablicy zerami

  for (i <- 0 to cols - 1) {                                                                                            //  Iteracja po kolumnach
    var current_min = Int.MaxValue

    for (j <- 0 to rows - 1) {                                                                                          //  Iteracja po wierszach
      current_min = math.min(current_min, matrix(j)(i))
    }

    result(i) = current_min
  }

  result
}


//  Przykładowa macierz prostokątna
val example_3d_array = Array(
  Array(23, 42, 12, -19, 20),
  Array(1, 5, 4, 7, 7),
  Array(51, 31, -2, -2, 68),
  Array(76, 21, 2, 2, 0)
)

findMinFunc(example_3d_array)
findMinImper(example_3d_array)

//  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------

class Rownanie(private val factors: Array[Double] = Array()):
  //  Główna funkcja do rozwiązywania równania
  def rozwiaz(): Array[Double] = factors.length match {
    case 3 => solveQuadratic()
    case 2 => solveLinear()
    case _ if (checkEquation()) =>
      println("Brak implementacji rownania.")
      Array()
    case _ =>
      println(s"Nieprawidlowa tablica wspolczynnikow.")
      Array()
  }


  //  Funckja do rozwiązywania równań liniowych
  private def solveLinear(): Array[Double] = {
    val a = factors(0)
    val b = factors(1)

    if (a == 0)
      if (b == 0) throw new Exception("Nieskonczenie wiele rozwiazan.")
      else {
        println("Brak rozwiazan")
        Array()
      }
    else
      val x = -b / a

      Array.apply(x)
  }


  //  Funckja do rozwiązywania równań kwadratowych
  private def solveQuadratic(): Array[Double] = {
    val a = factors(0)
    val b = factors(1)
    val c = factors(2)

    if (a == 0) solveLinear()

    val delta = math.pow(b, 2) - 4 * a * c

    if (delta < 0) {
      println("Brak rozwiazan rownania.")
      Array()
    }
    else if (delta == 0) {
      val x = -b / (2 * a)

      Array.apply(x)
    }
    else {
      val x1 = (-b - math.sqrt(delta)) / (2 * a)
      val x2 = (-b + math.sqrt(delta)) / (2 * a)

      Array.apply(x1, x2)
    }
  }


  //  Funkcja pomocnicza do sprawdzania poprawności wprowadzonej tablicy współczynników
  private def checkEquation(): Boolean = {
    var factorSum = 0.0

    for (i <- 0 to factors.length - 3) {
      factorSum += factors(i)
    }

    //  Sprawdzany warunek:
    //  Tablica współczynników dłuższa niż 3
    //  i na pozycjach od 0 do factors.length - 3
    //  są współczynniki różne od 0
    (factorSum != 0) && (factors.length > 3)
  }

end Rownanie


// Nieprawidłowa liczba współczynników (0)
val rownaznieZle = new Rownanie(Array())
rownaznieZle.rozwiaz()

// Nieprawidłowa liczba współczynników (1)
val rownanieZero = new Rownanie(Array(1))
rownanieZero.rozwiaz()

// Równanie liniowe 2x + 4 = 0 (rozw: -2)
val rownanieLiniowe = new Rownanie(Array(2, 4))
rownanieLiniowe.rozwiaz()

// Równanie kwadratowe x^2 - 5x + 6 = 0 (rozw: 2, 3)
val rownanieKwadratowe = new Rownanie(Array(1, -5, 6))
rownanieKwadratowe.rozwiaz()

// Równanie kwadratowe 5.5x^2 + 15.2x + 2.27 = 0
val rownanieKwadratowe2 = new Rownanie(Array(5.5, 15.2, 2.27))
rownanieKwadratowe2.rozwiaz()

// Równanie kwadratowe x^2 + 4x + 5 = 0 (brak rozwiązań rzeczywistych)
val rownanieBezRozwiazania = new Rownanie(Array(1, 4, 5))
rownanieBezRozwiazania.rozwiaz()

// Przykład równań wyższego stopnia (brak implementacji)
val rownanieWyzszegoStopnia = new Rownanie(Array(1, 2, 3, 4))
rownanieWyzszegoStopnia.rozwiaz()
