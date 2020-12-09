object Day9 extends App {

  def returnFirstInvalid(preambleLength : Int, previousNumbers : List[Long], remaining : List[Long]) : Option[Long] = {
    remaining match {
      case Nil => None
      case x :: newRemaining => {
        if (previousNumbers.combinations(2).exists(_.sum == x)) returnFirstInvalid(preambleLength, x :: previousNumbers.take(preambleLength -1), newRemaining)
        else Some(x)
      }
    }
  }

  def findContiguousSequence(contiguous : Int, target : Long, input : List[Long]) : List[Long] = {
    if (contiguous > input.size) List.empty
    else input.sliding(contiguous).find(_.sum == target) match {
      case None => findContiguousSequence(contiguous + 1, target, input)
      case Some(found) => found
    }
  }

  def findAnswer1(preambleLength : Int, input : List[Long]) : Long = {
    returnFirstInvalid(preambleLength, input.take(preambleLength).reverse, input.drop(preambleLength)).getOrElse(-1)
  }

  def findAnswer2(preambleLength : Int, input : List[Long]) : Long = {
    val invalid = findAnswer1(preambleLength, input)
    val smallestContiguousSequence = findContiguousSequence(2, invalid, input)
    smallestContiguousSequence.min + smallestContiguousSequence.max
  }

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day9TestData"
  val lines = scala.io.Source.fromFile(fileName).getLines().toList.map(_.toLong)

  println(findAnswer1(25, lines))
  println(findAnswer2(25, lines))
}
