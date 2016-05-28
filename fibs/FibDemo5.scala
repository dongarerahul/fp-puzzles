package fibs

// 1, 1, 2, 3, 5, 8, 13
/**
 * Somewhat trying to achieve Elegance
 * Still manually passing memo around
 * Not so clean
 */

object FibDemo5 {

  type Memo = Map[BigInt, BigInt]

  def fib(n: BigInt): BigInt = {
    def fibR(z: BigInt): State[BigInt, Memo] =
      if(z <= 1)
        State.insert(z)
      else
        for {
          u <- State.get((m: Memo) => m get z)
          v <- u map State.insert[BigInt, Memo] getOrElse(for {
            r <- fibR(z - 1)
            s <- fibR(z - 2)
            t = r + s
            _ <- State.modify((m: Memo) => m + ((z, t)))
          } yield t)
        } yield v

    fibR(n) eval Map()
  }

  def main(args: Array[String]) {
    println("Efficient & Elegant Only Fib: " + fib(7))
  }
}
