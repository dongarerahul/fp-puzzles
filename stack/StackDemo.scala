package stack

import scala.collection.immutable

case class StateM[S, A](run: S => (S, A)) {
  def getState = {
    StateM[S, S] { (s: S) => (s, s) }
  }

  def getS[S, A] (f: S => A) = {
    StateM[S, A] { (s: S) => (s, f(s))}
  }

  def setState(newState: S) = StateM[S, A] { (s: S) =>
    val (_, a) = run(s)
    (newState, a)
  }

  def map[B](f: A => B) : StateM[S, B] = StateM {(s: S) =>
    val (newState, a) = run(s)
    (newState, f(a))
  }

  def flatMap[B](f: A => StateM[S, B]) = StateM[S, B] { (s: S) =>
    val (newState, a) = run(s)
    f(a).run(newState)
  }

  def modify(f: S => S) : StateM[S, Unit] = {
    StateM { (s: S) => (f(s), ()) }
  }
}

object StateM {
  def units[S, A](f: S => A) : StateM[S, A] = StateM { (s: S) => (s, f(s)) }
  def unit [S, A](a: A)      : StateM[S, A] = units {(_: S) => a}

  def unitSimple[S, A](a: A)       : StateM[S, A] = StateM { (s: S) => (s, a)}
}

object StackDemo {

  def push(x: Int) = StateM[List[Int], Unit] { (s: List[Int]) => s match {
      case xs => ((x :: xs), ())
    }
  }

  def pop() : StateM[List[Int], Option[Int]] = StateM { (s: List[Int]) => s match {
    case (x :: xs) => (xs, Some(x))
    case xs        => (xs, None)
  }}
}

object Main {
  def push3pop1(initialState: StateM[List[Int], Unit]) = {
    /*val state1 = initialState.flatMap(_ => StackDemo.push(1)) //: StateM[List[Int], Unit]
    val state2 = state1.flatMap(_ => StackDemo.push(2))
    val state3 = state2.flatMap(_ => StackDemo.push(3))

    state3.flatMap(_ => StackDemo.pop())*/

    initialState
      .flatMap(_ => StackDemo.push(1)) //: StateM[List[Int], Unit]
      .flatMap(_ => StackDemo.push(2))
      .flatMap(_ => StackDemo.push(3))
      .flatMap(_ => StackDemo.pop())
      //.modify(_ => List(100, 101))

    /*for {*/
    /*  _ <- StackDemo.push(1)*/
    /*  _ <- StackDemo.push(2)*/
    /*  _ <- StackDemo.push(3)*/
    /*  x <- StackDemo.pop()*/
    /* } yield x*/
  }

  def main(args: Array[String]) {
    println("Starting ...")
    val computed = push3pop1(StateM.unit[List[Int], Unit]())
    val(s, a) = computed.run(List.empty[Int])

    println(s)
    println(a)

    assert(s == List(2, 1))
    assert(a == Some(3))
  }
}

