case class Instruction(command : Char, amount : Long)

object Instruction {
  def fromInput(input : String) = {
    Instruction(input.head, input.tail.toLong)
  }
}

sealed trait Direction {
  def move(moveable: Moveable, distance : Long) : Moveable
  def turnLeft : Direction
  def turnRight : Direction
}

case object North extends Direction {
  override def move(moveable: Moveable, distance : Long) : Moveable = moveable.setLatitude(moveable.latitude - distance)

  override def turnLeft: Direction = West

  override def turnRight: Direction = East
}

case object West extends Direction {
  override def move(moveable: Moveable, distance : Long) : Moveable= moveable.setLongitude(moveable.longitude - distance)

  override def turnLeft: Direction = South

  override def turnRight: Direction = North
}

case object East extends Direction {
  override def move(moveable: Moveable, distance : Long) : Moveable = moveable.setLongitude(moveable.longitude + distance)

  override def turnLeft: Direction = North

  override def turnRight: Direction = South
}

case object South extends Direction {
  override def move(moveable: Moveable, distance: Long): Moveable = moveable.setLatitude(moveable.latitude + distance)

  override def turnLeft: Direction = East

  override def turnRight: Direction = West
}

sealed trait Moveable {
  def longitude : Long
  def latitude : Long
  def setLongitude(value : Long) : Moveable
  def setLatitude(value : Long) : Moveable
}

abstract class Ferry() extends Moveable {
  def processInstruction(instruction : Instruction) : Moveable
  def forward(distance : Long) : Moveable

}

case class Ferry1(longitude : Long, latitude : Long, facing : Direction) extends Ferry {

  def forward(distance : Long) : Moveable = facing.move(this, distance)

  def processInstruction(instruction : Instruction) : Moveable = {
    instruction.command match {
      case 'F' => forward(instruction.amount)
      case 'L' => this.copy(facing = (1L to instruction.amount / 90L).foldLeft(this.facing)((direction,_) => direction.turnLeft))
      case 'R' => this.copy(facing = (1L to instruction.amount / 90L).foldLeft(this.facing)((direction,_) => direction.turnRight))
      case 'N' => North.move(this, instruction.amount)
      case 'E' => East.move(this, instruction.amount)
      case 'S' => South.move(this, instruction.amount)
      case 'W' => West.move(this, instruction.amount)
    }
  }

  override def setLongitude(value: Long): Moveable = this.copy(longitude = value)

  override def setLatitude(value: Long): Moveable = this.copy(latitude = value)
}

case class Waypoint(longitude: Long, latitude : Long) extends Moveable {
  override def setLongitude(value: Long): Moveable = this.copy(longitude = value)

  override def setLatitude(value: Long): Moveable = this.copy(latitude = value)

  def rotateLeft: Waypoint = Waypoint(this.latitude, -1 * this.longitude)
  // 1, -2 => -2, -1 => -1, 2 => 2, 1 => 1, -2

  def rotateRight: Waypoint = Waypoint(-1 * this.latitude, this.longitude)
  // 1, -2 => 2, 1 => -1, 2 => -2, -1 => 1, -2
}

case class Ferry2(longitude : Long, latitude : Long, waypoint : Waypoint) extends Ferry {

  override def setLongitude(value: Long): Moveable = this.copy(longitude = value)

  override def setLatitude(value: Long): Moveable = this.copy(latitude = value)

  def setWaypoint(newWaypoint : Waypoint) : Ferry2 = this.copy(waypoint = newWaypoint)

  override def processInstruction(instruction: Instruction): Moveable = {
    instruction.command match {
      case 'F' => forward(instruction.amount)
      case 'L' => setWaypoint((1L to instruction.amount / 90L).foldLeft(this.waypoint)((wp, _) => wp.rotateLeft))
      case 'R' => setWaypoint((1L to instruction.amount / 90L).foldLeft(this.waypoint)((wp, _) => wp.rotateRight))
      case 'N' => setWaypoint(North.move(this.waypoint, instruction.amount).asInstanceOf[Waypoint])
      case 'E' => setWaypoint(East.move(this.waypoint, instruction.amount).asInstanceOf[Waypoint])
      case 'S' => setWaypoint(South.move(this.waypoint, instruction.amount).asInstanceOf[Waypoint])
      case 'W' => setWaypoint(West.move(this.waypoint, instruction.amount).asInstanceOf[Waypoint])
    }
  }

  override def forward(distance: Long): Moveable = this.copy( latitude = this.latitude + waypoint.latitude * distance, longitude = this.longitude + waypoint.longitude * distance)
}

object Day12 extends App {

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day12TestData"
  val instructions = scala.io.Source.fromFile(fileName).getLines().toList.map(Instruction.fromInput)


  def findAnswer1(instructions : List[Instruction]): Long = {
    val finalPosition = instructions.foldLeft[Ferry](Ferry1(0L, 0L, East))(
      (ferry, instruction) => {
        ferry.processInstruction(instruction).asInstanceOf[Ferry1]
      }
    )
    finalPosition.latitude + finalPosition.longitude
  }

  def findAnswer2(instructions : List[Instruction]): Long = {
    val finalPosition = instructions.foldLeft[Ferry](Ferry2(0L, 0L, Waypoint(10L, -1L)))(
      (ferry, instruction) => {
        println(ferry, instruction)
        ferry.processInstruction(instruction).asInstanceOf[Ferry2]
      }
    )
    math.abs(finalPosition.latitude) + math.abs(finalPosition.longitude)
  }

  println(findAnswer1(instructions))
  println(findAnswer2(instructions))
}
