object Day10 extends App {
  def getGaps(chargers : List[Long]):List[Long] = chargers.sorted.sliding(2).map(pair => pair.max - pair.min).toList

  def getGapMap(chargers : List[Long]) : Map[Long, Long] = getGaps(chargers).groupBy(a => a).map(a => (a._1, a._2.size))

  def getWaysICouldGoForward(from: Long, remaining : List[Long]) : Long = {
    remaining match {
      case Nil => 1
      case _ => {
        val options = remaining.takeWhile(_ <= from + 3L)
          options.map(
            newTarget => getWaysICouldGoForward(newTarget, remaining.dropWhile(_ <= newTarget))).sum
      }
    }
  }

  def breakdownIntoThreeGapped(current : List[Long], remaining : List[Long]): List[List[Long]] =
  {
    remaining match {
      case Nil => List(current.reverse)
      case x :: xs if x - current.head == 3L => current.reverse :: breakdownIntoThreeGapped(List(x), xs)
      case x :: xs => breakdownIntoThreeGapped(x :: current, xs)
    }
  }

  def findAnswer1(input: List[Long]): Long = {
    val mappedGaps = getGapMap(0L :: input)
    mappedGaps(1L) * (mappedGaps(3L) + 1L)
  }

  def findAnswer2(input: List[Long]): Long = {
    val threeGapped = breakdownIntoThreeGapped(List(0L), input.sorted)
    val subResults = threeGapped.map(subList => getWaysICouldGoForward(subList.head, subList.tail))
    subResults.foldLeft(1L)(_ * _)
  }

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day10TestData"
  val lines = scala.io.Source.fromFile(fileName).getLines().toList.map(_.toLong)

  println(findAnswer1(lines))
  println(findAnswer2(lines))
}
