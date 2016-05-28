package robo

case class Score(player: Player, score: Int)

case class PlayGround(bottomLeft: Point, topRight: Point, coins: Set[Point], r1: Robot, r2: Robot) {

  def isInPlayground(point: Point): Boolean = {
    bottomLeft.x <= point.x
  }

  def isPossiblePosition(position: Position): Boolean = {
    if(r1.currentPosition.point == position.point) false else true
  }

  def swapRobots() : PlayGround = copy(r1 = r2, r2 = r1)

  lazy val scores = (r1.score, r2.score)

}

case class Point(x: Int, y: Int)

case class Position(point: Point, direction: Direction) {
  
  def move(s: PlayGround) : Position = {
    val p1 = direction match {
      case North => copy(point = point.copy(y = point.y + 1))
      case South => copy(point = point.copy(y = point.y - 1))
      case East  => copy(point = point.copy(x = point.x + 1))
      case West  => copy(point = point.copy(x = point.x - 1))
    }
    if(s.isPossiblePosition(p1)) p1 else this
  }

  def turn(instruction: Instruction) : Position = {
    copy(direction = direction.turn(instruction))
  }
}
