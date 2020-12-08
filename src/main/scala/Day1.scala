object Day1 extends App {
  def findSumToTwentyTwenty(howMany : Int, candidates : List[Int]) : Option[List[Int]] = {
    candidates.combinations(howMany).filter(_.sum == 2020).toList.headOption
  }

  def findAnswer(n : Int, listOfCandidates : List[Int]) : Int = {
    findSumToTwentyTwenty(n, listOfCandidates).map(_.product).getOrElse(-1)
  }

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day1TestData"
  val list = scala.io.Source.fromFile(fileName).getLines().toList.flatMap(_.toIntOption)
  println(findAnswer(2,list))
  println(findAnswer(3,list))
}
