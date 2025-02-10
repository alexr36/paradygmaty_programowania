package Zad5

class Main


object Main extends App {
  //  Hamlet by William Skaespeare - Act 1, Scene 1 - Synopsis
  private val text =
    "Synopsis:\n\nOn the guards’ platform at Elsinore, " +
    "Horatio waits with Barnardo and Marcellus to question a ghost that has twice before appeared. " +
    "The Ghost, in the form of the late King Hamlet of Denmark, appears but will not speak. " +
    "Horatio decides to tell his fellow student, Prince Hamlet, about the Ghost’s appearance."
  private val word_counter = new WordCounter()

  println(word_counter.wordCounter(text))
}
