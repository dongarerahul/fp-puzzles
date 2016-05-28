package functors.demo

//import org.scalacheck.Prop
//import org.specs.{ScalaCheck, Specification}

class ListFunctorTest { //extends Specification with ScalaCheck {

  val listFunctor = new Functor[List] {
    override def fmap[A, B](fa: List[A])(f: A => B): List[B] = {
      fa map f
    }
  }

  /*"A List Functor should" {
    "Preserve identity" in {
      val stringID = (s: String) => s
      val listId = (ss: List[String]) => ss

      Prop forAll { (ss: List[String]) =>
        listFunctor.fmap(ss)(stringID) == listId(ss)
      } must pass
    }
  }*/
}
