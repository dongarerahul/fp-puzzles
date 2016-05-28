package fibs

// 1, 1, 2, 3, 5, 8, 13
/**
 * Somewhat trying to achieve Elegance
 * Still manually passing memo around
 * Not so clean
 */

object FibDemo3 {

  type Memo = Map[BigInt, BigInt]

  def fib(n: BigInt): BigInt = {

    def fibMemo(z: BigInt): State[BigInt, Memo] = {
      State(memo =>
        if (z <= 1)
          (z, memo)
        else memo get (z) match {
          case Some(v) => (v, memo)
          case None =>
            val (r, memo1) = fibMemo(z - 1) run(memo)
            val (s, memo2) = fibMemo(z - 2) run(memo1)
            (r + s, memo2)
        }
      )
    }
    fibMemo(n).run(Map())._1
  }

  def main(args: Array[String]) {
    println("Efficient & Somewhat Elegant Fib: " + fib(7))
    /*println("Testing of State Monad")

    type Memo = Map[Int, Int]
    val s1 = State.insert(10)
    val s2 = State.insert(20)
    val v = State.get((memo: Memo) => memo.get(10))
    //println(v.eval())*/

  }
}

case class State[A, S](run: S => (A, S)) {
  // State => (vAl, newState)

  def map[B](f: A => B): State[B, S] = {
    State { s =>
      val (a, t) = run(s)
      (f(a), t)
    }
  }

  def flatMap[B](f: A => State[B, S]): State[B, S] = {
    State { s =>
      val (a, t) = run(s)
      f(a) run t
    }
  }

  def eval(s: S) : A = run(s)._1
}

object State {
  def insert[A, S](a: A) : State[A, S] = State(s => (a, s))
  def get[A, S](f: S => A) : State[A, S] = State(s => (f(s), s))
  def modify[S](f: S => S) : State[Unit, S] = State(s => ((), f(s)))
}
