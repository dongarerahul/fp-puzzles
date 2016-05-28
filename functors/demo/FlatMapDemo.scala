package functors.demo

object FlatMapDemo {

  def main (args: Array[String]) {
    val list1 = List(1, 2, 3)
    val list2 = List(91, 92, 93)

    print ("Using For Loop:\t")
    val tuple = for(x <- list1; y <- list2) print ("[" + x + ", " + y + "] ")

    print("\nUsing ForEach: \t")
    list1 foreach { x => list2 foreach { y => print ("[" + x + ", " + y + "] ") } } // will print 9 combinations

    print("\nUsing FlatMap: \t")
    list2.flatMap(y => list1 map (x => print ("[" + x + ", " + y + "] ")))

  }
}
