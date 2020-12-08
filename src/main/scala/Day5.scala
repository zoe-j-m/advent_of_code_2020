import scala.annotation.tailrec

trait SeatLocation {
  def apply(char : Char) : SeatLocation
  def seatId : Int
}

case class DefiniteSeatLocation(row: Int, column:Int) extends SeatLocation {
  override def apply(char: Char) = this

  override def seatId: Int = row * 8 + column
}

case class PossibleSeatLocation(rows : Range, columns: Range) extends SeatLocation {
  override def apply(char: Char) = {
    val (newRows, newCols) = char match {
      case 'F' => (rows.start to rows.`end`  - rows.size/2, columns)
      case 'B' => (rows.start + rows.size / 2 to rows.`end` , columns)
      case 'L' => (rows, columns.start to columns.`end` - columns.size/2)
      case 'R' => (rows, columns.start + columns.size/2 to columns.end)
    }
    if ((newRows.size == 1) && (newCols.size == 1))  DefiniteSeatLocation(newRows.start, newCols.start)
    else PossibleSeatLocation(newRows, newCols)
  }
  override def seatId: Int = throw new Exception("Incomplete seat designation")
}

object SeatLocation {
  val startingSeat = PossibleSeatLocation(0 to 127, 0 to 7)
  def op(location : SeatLocation, direction : Char) : SeatLocation =  location.apply(direction)
  def fromInput(input : String) = input.foldLeft[SeatLocation](startingSeat)(op)
}

object Day5 extends App {
  val a = 0 to 127

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day5TestData"
  val lines = scala.io.Source.fromFile(fileName).getLines().toList

  def findAnswer1(input: List[String]) = input.map(SeatLocation.fromInput).map(_.seatId).max
  def findAnswer2(input: List[String]) = {
    val seats = input.map(SeatLocation.fromInput).collect {case x : DefiniteSeatLocation => x}
    println(seats.size)
    val possibleSeats = for {
      row <- 1 to 126
      column <- 0 to 7
    } yield (row, column)
    val remainingSeats = seats.foldLeft(possibleSeats.toList)((a,b) =>
      { println(a.size, b)
        a.filterNot(c => (b.row == c._1) && (b.column == c._2) )} )
    val seatIds = remainingSeats.map(a => a._1* 8 + a._2).sorted
    seatIds.filterNot(a => seatIds.contains(a -1) || seatIds.contains(a + 1))
  }
  println(findAnswer1(lines))
  println(findAnswer2(lines))
}
