sealed trait Square {
  def isOccupied : Boolean
  def occupy : Square
  def vacate : Square
}
case object Floor extends Square {
  override def isOccupied = false
  override def occupy: Square = this
  override def vacate: Square = this
}
case class Seat(isOccupied : Boolean) extends Square {
  override def occupy: Square = Seat(true)
  override def vacate: Square = Seat(false)
}

case class LayoutMode(occupiedThreshold : Int, countingMode : Int)

case class Layout(squares: List[List[Square]])(implicit mode : LayoutMode) {
  val maxI = squares.size - 1
  val maxJ = squares.head.size - 1

  def getOccupied(i : Int, j: Int) : Boolean = {
    if (i<0 || i > maxI || j < 0 || j> maxJ) false else squares(i)(j).isOccupied
  }

  def getOccupiedSurroundingSeatsToThreshold(i : Int, j: Int, threshold : Int) : Boolean = {
    val occupied =  for {
      i1 <- i -1 to i +1
      j1 <- j -1 to j +1
      if i1 != i || j1 != j
    } yield getOccupied(i1,j1)
    occupied.count(a => a) >= threshold
  }

  def getFirstSeatInDirection(i : Int, j: Int, iDelta : Int, jDelta : Int) : Option[Square] = {
    if (i < 0 || i > maxI || j < 0 || j > maxJ) None
    else if (squares(i)(j) == Floor) getFirstSeatInDirection(i + iDelta, j+ jDelta, iDelta, jDelta)
    else Some(squares(i)(j))
  }

  def getOccupiedViewableSeatsToThreshold(i: Int, j: Int, threshold : Int) : Boolean = {
    val seats =  for {
        id <-  -1 to  +1
        jd <-  -1 to  +1
        if id != 0 || jd != 0
      } yield  getFirstSeatInDirection(i + id, j + jd, id, jd)
    seats.flatten.count(_.isOccupied) >= threshold
}

  def permuteSquare(i: Int, j: Int) : Square = {
    val square = squares(i)(j)
    val thresholdCheck : (Int, Int, Int) => Boolean = if (mode.countingMode == 0) getOccupiedSurroundingSeatsToThreshold else getOccupiedViewableSeatsToThreshold
    if (!square.isOccupied && !thresholdCheck(i, j,1)) square.occupy
    else if (square.isOccupied && thresholdCheck(i,j, mode.occupiedThreshold)) square.vacate
    else square
  }

  def permuteIteration: Layout = {
    Layout( (for {
      i <- squares.indices
      j <- squares(i).indices
      } yield permuteSquare(i, j)
    ).toList.grouped(squares.head.size).toList)
  }

  def countOccupied : Int = {
    squares.map(_.count(_.isOccupied)).sum
  }
}

object Layout{
  def fromInput(input: List[String])(implicit mode : LayoutMode) : Layout = {
    Layout(input.map(
      _.toList.map {
        case '.' => Floor
        case 'L' => Seat(false)
        case '#' => Seat(true)
      }
    )
    )
  }
}

object Day11 extends App {

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day11TestData"
  val lines = scala.io.Source.fromFile(fileName).getLines().toList

  def permuteUntilStable(layout: Layout): Int = {
    val permutedLayout = layout.permuteIteration
    if (permutedLayout == layout) layout.countOccupied else permuteUntilStable(permutedLayout)
  }

  def findAnswer1(lines : List[String]) : Int = {
    implicit val mode = LayoutMode(4, 0)
    permuteUntilStable(Layout.fromInput(lines))
  }

  def findAnswer2(lines : List[String]) : Int = {
    implicit val mode = LayoutMode(5, 1)
    permuteUntilStable(Layout.fromInput(lines))
  }

  println(findAnswer1(lines))
  println(findAnswer2(lines))
}
