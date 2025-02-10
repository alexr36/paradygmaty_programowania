package Zad2

class Main


object Main extends App {
  //  Created object is of its class type (not parametrized)
  //  In this case: ConcretePolymorphicPair
  val polymorphic_pair = new ConcretePolymorphicPair()

  polymorphic_pair.fst = 4
  polymorphic_pair.snd  = "ABCdef"

  println(s"Fst: ${ polymorphic_pair.fst }")
  println(s"Snd: ${ polymorphic_pair.snd }")
  println(s"Pair: $polymorphic_pair")
}
