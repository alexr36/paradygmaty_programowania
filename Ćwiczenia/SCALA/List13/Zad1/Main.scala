package Zad1

class Main


object Main extends App {
  //  Created object is of exact type specified in the constructor 
  //  In this case: PolymorphicPair[Int, String]
  val polymorphic_pair = new PolymorphicPair[Int, String]

  polymorphic_pair.fst = 4
  polymorphic_pair.snd  = "ABCdef"

  println(s"Fst: ${ polymorphic_pair.fst }")
  println(s"Snd: ${ polymorphic_pair.snd }")
  println(s"Pair: $polymorphic_pair")
}
