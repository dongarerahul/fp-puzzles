package dictionary

import stack.StateM

object DictionaryStateDemo {

  type Log = String
  type Dictionary = Map[String, Any]

  val dictionary:Dictionary = Map("key1"->1, "key2"->2, "key3"->"value3")

  def transform(key: String, value: Any, currentLog: String = "") : StateM[Dictionary, Log] = {
    StateM { dict =>
      val newDict = dict + (key -> value)
      (newDict, currentLog ++ s"| Added $key : $value ")
    }
  }

  def main(args: Array[String]) {

    //method#1
    val state1 = transform("key4", 0)

    //method#2
    val state2 = state1.flatMap(log => transform("key5", true, log))

    //method#3
    val state3 = for {
      s1 <- transform("key6", 6.0, "")
      s2 <- transform("key7", 7.0, s1)
    } yield s2

    println(state1.run(dictionary))
    println(state2.run(dictionary))
    println(state3.run(dictionary))
  }
}
