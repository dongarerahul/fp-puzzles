package fibs

// 1, 1, 2, 3, 5, 8, 13
/**
 * Somewhat trying to achieve Elegance
 * Still manually passing memo around
 * Not so clean
 */

object FibDemo4 {

  type Memo = Map[BigInt, BigInt]

  def fib(n: BigInt): BigInt = {
    def fibR(z: BigInt): State[BigInt, Memo] =
      if(z <= 1)
        State.insert(z)
      else
        for {
          u <- State.get((m: Memo) => m get z)
          v <- u map State.insert[BigInt, Memo] getOrElse
            fibR(z - 1) flatMap (r =>
              fibR(z - 2) flatMap (s => {
                val t = r + s
                State.modify((m: Memo) => m + ((z, t))) map (_ => t)
              }))
        } yield v

    fibR(n) eval Map()
  }

  def main(args: Array[String]) {
    println("Efficient & Elegant Fib: " + fib(6))
  }
}
