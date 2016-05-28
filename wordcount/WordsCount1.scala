package wordcount

object WordsCount1 {

  def main(args: Array[String]) {
    val lines =
      """ This is line one
        | This is line two
        | This is line three
      """.stripMargin;

    val result: Map[String, Int] = wordCounts(lines)
    println(result)
  }

  /// gives list of words from multiple lines
  def words(lines: String) : List[String] = {
    val toList: List[String] = lines.split("\n").toList
    toList.flatMap(_.split("\\W+"))
  }

  def wordCounts(lines: String) : Map[String, Int] = {
    words(lines).foldLeft(Map.empty[String, Int]) { (map, word) =>
      val count = map.getOrElse(word, 0) + 1
      map + (word -> count)
    }
    //list.groupBy((word: String) => word).mapValues(_.length)
  }
}
