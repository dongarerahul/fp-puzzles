package wordcount

import stack.StateM

object WordsCount5 {

  case class Article(headline: String, abstracts: String, body: String)

  def main(args: Array[String]) {

    val a = new Article (
      "This is a headline",
      "This is a abstract",
      "This is a body"
    )

    val articles = List(a, a)
    val states: List[StateM[Map[String, Int], Unit]] = articles map wordCountForArticle

    val initialState = StateM { (s: Map[String, Int]) => (s, ()) }

    println("Article Word Count: " + states);
  }

  /// gives list of words from multiple lines
  def words(lines: String) : List[String] = {
    val toList: List[String] = lines.split("\n").toList
    toList.flatMap(_.split("\\W+"))
  }

  def wordCount(lines: String) : StateM[Map[String, Int], Unit] = {
    type ResultS = StateM[Map[String, Int], Unit]
    val initial : ResultS = StateM.unitSimple(())

    initial.modify { currentMap: Map[String, Int] =>
      words(lines).foldLeft(currentMap){ (map, word) =>
        val count = map.getOrElse(word, 0) + 1
        map + (word -> count)
      }
    }
  }

  def wordCountForArticle(article: Article) = for {
    _ <- wordCount(article.headline)
    _ <- wordCount(article.abstracts)
    _ <- wordCount(article.body)
  } yield ()
}
