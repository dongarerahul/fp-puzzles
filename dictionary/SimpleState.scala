import robo.State

object SimpleState {
  /*type Plus = Int => Int

  def create(no: Int) :  Plus = { (n: Int) => no + n }

  def main(args: Array[String]) {
    val p5 = create(5)
    val p2 = create(2)

    create(0).flatMap(p5)(0)
    println("Calculating Sum: " + p2(0))
    println("Calculating Sum: " + p5(5))
  }*/

  val m1 = State {
    s: String => (s, s.size)
  }

  def repeat(n: Int) : State[String, Unit] = State { s: String => (s * n, ()) }

  def main(args: Array[String]) {

    println("Simple State Application ...")
    println("Run: " + m1.run("abc"))
    println("Repeat: " + repeat(3).run("ABC "))

    /**
     *
     */
    println("-------------")
    val s: State[String, Unit] = m1.flatMap(repeat)
    println(s.run("AB "))

    val s1: State[String, Int] = m1.flatMap(repeat).flatMap(_ => m1)
    println(s1.run("AB "))
  }
}