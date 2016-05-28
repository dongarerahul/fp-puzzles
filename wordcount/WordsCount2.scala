package wordcount

object WordsCount2 {

  object Article {
    val headline  = "This is a headline"
    val abstracts = "This is a abstract"
    val body      = "This is a body"
  }

  /**
   * Problem here is we are storing intermediate results (state) result1, 2, 3 which
   * are not needed and can be abused to produce wrong end result.
   *
   * Hence in the next version, we will try to remove those using state monad
   *
   * @param args
   */
  def main(args: Array[String]) {
    val map = Map[String, Int]() // This map carries state i.e. intermediate results
    val result1: Map[String, Int] = wordCounts1(Article.headline, map)
    val result2: Map[String, Int] = wordCounts1(Article.abstracts, result1)
    val result3: Map[String, Int] = wordCounts1(Article.body  , result2)

    println("Using map state: " + result3)
  }

  /// gives list of words from multiple lines
  def words(lines: String) : List[String] = {
    val toList: List[String] = lines.split("\n").toList
    toList.flatMap(_.split("\\W+"))
  }

  def wordCounts1(lines: String, currentMap: Map[String, Int]) : Map[String, Int] = {
    words(lines).foldLeft(currentMap) { (map, word) =>
      val count = map.getOrElse(word, 0) + 1
      map + (word -> count)
    }
  }
}
