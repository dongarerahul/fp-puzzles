package robo

sealed trait Instruction
case object L extends Instruction
case object R extends Instruction
case object A extends Instruction

sealed trait Direction {
  def turn(i: Instruction) : Direction
}

case object West extends Direction {
  override def turn(i: Instruction): Direction = i match {
    case L => South
    case R => North
    case _ => this
  }
}
case object South extends Direction {
  override def turn(i: Instruction): Direction = i match {
    case L => East
    case R => West
    case _ => this
  }
}
case object East extends Direction {
  override def turn(i: Instruction): Direction = i match {
    case L => North
    case R => South
    case _ => this
  }
}
case object North extends Direction {
  override def turn(i: Instruction): Direction = i match {
    case L => West
    case R => East
    case _ => this
  }
}
