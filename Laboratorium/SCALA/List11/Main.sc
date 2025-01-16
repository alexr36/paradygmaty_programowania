object Main extends App {
  // Tworzenie zwierząt
  val cow_1 = Zwierzak("Krowa", "Basia", 2015)
  val horse_1 = Zwierzak("Kon", "Blyskawica", 2018)
  val goat_1 = Zwierzak("Koza", "Kasia", 2020)

  // Tworzenie obór
  val byre_1 = new Obora("Jan Kowalski", 2)
  val byre_2 = new Obora("Anna Nowak", 3)

  // Operacje na oborach
  byre_1.addAnimal(cow_1)
  byre_1.addAnimal(horse_1)
  byre_1.addAnimal(goat_1) // Brak miejsca
  byre_2.addAnimal(goat_1)

  byre_1.showAnimals()
  byre_2.showAnimals()

  byre_1.showAnimals()

  byre_1.moveAnimal(cow_1, byre_2)
  byre_1.showAnimals()
  byre_2.showAnimals()
}