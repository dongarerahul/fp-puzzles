package fibs

// 1, 1, 2, 3, 5, 8, 13
/** *
  * We added memo cache here to store intermediate results
  * and avoid duplicate fib calculations
  * but we are sacrificing elegance here
  */
object FibDemo2 {

  def fib(n: BigInt): BigInt = {
    type Memo = Map[BigInt, BigInt]

    def fibMemo(z: BigInt, memo: Memo): (BigInt, Memo) = {
      if (z <= 1)
        (z, memo)
      else memo get (z) match {
        case None =>
          val (r, memo1) = fibMemo(z - 1, memo)

          val (s, memo2) = fibMemo(z - 2, memo1)

          (r + s, memo2)

        case Some(v) => (v, memo)
      }
    }
    fibMemo(n, Map())._1
  }

  def main(args: Array[String]) {
    println("Efficient Fib: " + fib(7))
  }
}
