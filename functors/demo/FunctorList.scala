package functors.demo

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B) : F[B]
}

object FunctorList {

  /**
   * Here we say that
   * type constructor like List is a Functor and below
   * Functor[F] constitutes proof of that
   */
  val listFunctor = new Functor[List] {
    override def fmap[A, B](fa: List[A]) (f: A => B): List[B] = {
      fa map f
    }
  }

  def main (args: Array[String]) {

    val listInt = List(1, 2, 3, 4, 5)
    val list = List("Welcome", "To", "World", "Of")
    val capitalize = (a: String) => a.toUpperCase()

    val result = listFunctor.fmap[Int, Int](listInt)(_ + 1)
    println(result)

    val result1 = listFunctor.fmap(list)(capitalize)
    println(result1)
  }
}

