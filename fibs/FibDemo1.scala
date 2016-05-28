package fibs

// 1, 1, 2, 3, 5, 8, 13
/* problem here is duplicate calculations of fib
= fib(3) + fib(2)
= (fib(2) + fib(1)) + (fib(1) + fib(0))
= ((fib(1) + fib(0)) + fib(1)) + (fib(1) + fib(0))
*/
object FibDemo1 {

  def fib(n: BigInt): BigInt =
    if (n <= 1)
      n
    else {
      val r = fib(n - 1)
      val s = fib(n - 2)
      r + s
    }

  def main(args: Array[String]) {
    println("Inefficient Fib: " + fib(7))
  }
}
