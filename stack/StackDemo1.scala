package stack

case class StateM1[S, A](run : S => (S, A)) {
  def map[B](f: A => B) : StateM1[S, B] = StateM1[S, B] { (s: S) =>
    val (newState, a) = run(s)
    (newState, f(a))
  }

  def flatMap[B](f: A => StateM1[S, B]) : StateM1[S, B] = StateM1[S, B] { (s: S) =>
    val (newState, a) = run(s)
    f(a).run(newState)
  }
}

object StackInt {

  def pop() : StateM1[List[Int], Option[Int]] = StateM1[List[Int], Option[Int]] { s => s match {
      case (x :: xs) => (xs, Some(x))
      case xs        => (xs, None)
    }
  }
  
  def push(a: Int) = StateM1[List[Int], Unit] { s => s match {
      case xs => (a :: xs, ()) 
    }
  }
}

object StackDemo1 {
  def main(args: Array[String]) {
    println("My StackDemo")

    /*val result = for {
      _ <- StackInt.push(1)
      _ <- StackInt.push(2)
      _ <- StackInt.push(3)
      _ <- StackInt.pop()
      _ <- StackInt.push(4)
      _ <- StackInt.push(5)
      x <- StackInt.pop()
    } yield x*/

    //val initialState = StateM1[List[Int], Unit](_ => (List[Int](), Unit))

    val result = StackInt.push(1)
                             .flatMap(_ => StackInt.push(2))
                             .flatMap(_ => StackInt.push(3))
                             .flatMap(_ => StackInt.pop())
                             .flatMap(_ => StackInt.push(4))
                             .flatMap(_ => StackInt.push(5))
                             .flatMap(_ => StackInt.pop())


    val (s, a) = result.run(List.empty[Int])
    
    println(s)
    println(a)
  }
}
