package functors.demo

object FunctorZip {

  trait Functor[F[_]] {

    def fmap[A, B](a: F[A])(f: A => B): F[B]

    //unzip function
    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = {
      val a: F[A] = fmap(fab)(_._1)
      val b: F[B] = fmap(fab)(_._2)
      (a, b)
    }

    def codistribute[A, B](e: Either[F[A], F[B]]) : F[Either[A, B]] = {
      e match {
        case Left(fa)  => fmap(fa)(Left(_))
        case Right(fb) => fmap(fb)(Right(_))
      }
    }
  }

  /*val listF = new Functor[_] {
    override def fmap[A, B](a: List[(A, B)])(f: A => B): List[B] = a map f
  }*/

  def main(args: Array[String]) {


  }
}
