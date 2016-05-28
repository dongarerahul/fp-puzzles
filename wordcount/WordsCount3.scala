package wordcount

import stack.StateM

case class Article(headline: String, abstracts: String, body: String)

object WordsCount3 {

  def main(args: Array[String]) {

    val a = new Article (
      "This is a headline",
      "This is a abstract",
      "This is a body"
    )

    val countResult = wordCount(a.headline)
                        .flatMap(_ => wordCount(a.body)
                        .flatMap(_ => wordCount(a.abstracts)))

    println("Article Word Count: " + countResult);
  }

  /// gives list of words from multiple lines
  def words(lines: String) : List[String] = {
    val toList: List[String] = lines.split("\n").toList
    toList.flatMap(_.split("\\W+"))
  }

  def wordCount(lines: String) : Map[String, Int] = {
    type ResultS = StateM[Map[String, Int], Unit]
    val initial : ResultS = StateM.unitSimple(())

    val result = initial.modify { currentMap: Map[String, Int] =>
      words(lines).foldLeft(currentMap){ (map, word) =>
        val count = map.getOrElse(word, 0) + 1
        map + (word -> count)
      }
    }

    result.run(Map.empty)._1
  }

  def wordCountForArticle(article: Article) = for {
    _ <- wordCount(article.headline)
    _ <- wordCount(article.abstracts)
    _ <- wordCount(article.body)
  } yield ()
}
