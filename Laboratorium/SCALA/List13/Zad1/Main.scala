package List13.Zad1

class Main

object Main extends App {
  val sufficient_loafs_amount = 20

  val bakery = new Piekarnia(sufficient_loafs_amount)
  val baker = new Piekarz(bakery)
  val deliverer = new Dostawca(bakery)
  val collector = new Odbiorca(bakery)

  baker.start()
  deliverer.start()
  collector.start()
}
