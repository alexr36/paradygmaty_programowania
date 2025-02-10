package Zad5


class WordCounter {
  def wordCounter(text: String): scala.collection.mutable.Map[String, Int] = {
    val result = scala.collection.mutable.Map[String, Int]()
    val text_to_words = text.split(" ")

    text_to_words.foreach(word => {
      if (word.nonEmpty) {    //  Omits empty words
        result(word) = result.getOrElse(word, 0) + 1
      }
    })

    result
  }
}
