package robo

sealed trait Player
case object R1 extends Player { override def toString() = "Robot1"}
case object R2 extends Player { override def toString() = "Robot2"}

case class Robot (player: Player, positions: List[Position], coins: List[Point] = Nil) {
  lazy val currentPosition = positions.head
  val score = Score(player, 0)

  def addPosition(next: Position) = copy(positions = next::positions)
  def addCoin(coin: Point) = copy(coins = coin::coins)
}
