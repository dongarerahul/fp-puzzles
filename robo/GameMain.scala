package robo

import scala.collection.immutable.HashSet

object GameMain {

  def main(args: Array[String]) {

    //Create Game Board Setup
    val bottomLeft = Point(0, 0)
    val topRight = Point(3, 3)

    val coins = new HashSet[Point] + Point(0, 1) + Point(0, 3) + Point(1, 2) + Point(2, 2) + Point(3, 1)
    val r1 = Robot(R1, List[Position](Position(Point(0, 0), North)))
    val r2 = Robot(R2, List[Position](Position(Point(3, 3), South)))
    val ground = PlayGround(bottomLeft, topRight, coins, r1, r2)

    // INSTRUCTION SET : A => Ahead, R => Turn Right, L => Turn Left
    val i1 = List[Instruction](A, A, R, A, A, A)
    val i2 = List[Instruction](A, R, A, L, A, A)

    //Score
    val result: State[PlayGround, (String, (Position, Position))] = enhanceResult(i1, i2)

    //Start Game
    val (p, (s, (p1, p2))) : (PlayGround, (String, (Position, Position))) = result.run(ground)
    println(p.r1 + " Score: " + p.scores._1.score + " | Positions" + p.r1.score)
    println(p.r2 + " Score: " + p.scores._2.score + " | Positions" + p.r2.score)

    println("Player Won: " + s)
  }

  def processInstruction(i: Instruction)(p: PlayGround) : PlayGround = {
    val next = i match {
      case A => p.r1.currentPosition.move(p) //move ahead
      case i => p.r1.currentPosition.turn(i) //Instruction
    }

    if(p.coins.contains(next.point)) {
      p.copy(
        coins = p.coins - next.point,
        r1 = p.r1.addCoin(next.point).addPosition(next)
      )
    } else {
      p.copy(r1 = p.r1.addPosition(next))
    }
  }

  /** *
    * Two list of instructions are processed alternately
    * @param i1 : Instruction Set 1
    * @param i2 : Instruction Set 2
    * @return   : State { PlayGround State, Robot Scores }
    */
  def compileInstructions(i1: List[Instruction], i2: List[Instruction]) : State[PlayGround, (Score, Score)] = {
    i1 match {
      // if both instruction sets are empty, just return original scores of both robots
      case Nil if i2 == Nil => State.gets(playGround => playGround.scores)

      //if one instruction set is empty, swap robots (only r1 gets processed) and again start compileInstructions
      case Nil => State[PlayGround, (Score, Score)] {
          playGround => (playGround.swapRobots(), playGround.scores)
      }.flatMap(_ => compileInstructions(i2, i1))

      // if instruction set one is not empty, process first instruction of the set and start compileInstruction
      case  head::tail => State[PlayGround, (Score, Score)] { playGround =>
        val newPlayGround = processInstruction(head)(playGround)
        (newPlayGround.swapRobots(), newPlayGround.scores)
      }.flatMap(_ => compileInstructions(i2, tail))
    }
  }

  def getPositions(p: PlayGround) : (Position, Position) = {
    (p.r1.currentPosition, p.r2.currentPosition)
  }

  def declareWinners(scores: (Score, Score)) : String = {
    if(scores._1.score > scores._2.score) "Player1" else "Player2"
  }

  def enhanceResult(i1: List[Instruction], i2: List[Instruction]) : State[PlayGround, (String, (Position, Position))] = {
    for {
      scores <- compileInstructions(i1, i2)
      positions <- State.gets(getPositions)
    } yield (declareWinners(scores), positions)
  }
}
