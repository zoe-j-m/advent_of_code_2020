import scala.annotation.tailrec

object Day6 extends App {
  def findAnswer1(values: List[List[String]]) = {
    values.map(_.mkString("").toSet.size).sum
  }

  def findAnswer2(values: List[List[String]]) = {
    values.map(
      setOfAnswers => setOfAnswers.tail.foldLeft(setOfAnswers.head.toList)((a,b) => a.filter(b.toList.contains)).size
    ).sum
  }

  @tailrec
  def splitUpInput(lines : List[String], soFar : List[List[String]]) : List[List[String]] = {
    lines match {
      case Nil => soFar.reverse
      case _ => splitUpInput(lines.dropWhile(_ != "").drop(1), lines.takeWhile(_ != "" ) :: soFar)
    }
  }
  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day6TestData"
  val lines = scala.io.Source.fromFile(fileName).getLines().toList

  val brokenUpLines = splitUpInput(lines, List.empty)

  println(findAnswer1(brokenUpLines))
  println(findAnswer2(brokenUpLines))
}
