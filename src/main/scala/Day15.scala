
object Day15 extends App {

  def generate(lastSpoken : Int, turnMap : Map[Int, Int], turn : Int, target : Int) : Int= {
    val found = turnMap.get(lastSpoken).map(turn - _ - 1 ).getOrElse(0)
    if (target == turn) found else generate(found, turnMap + (lastSpoken -> (turn - 1)), turn + 1, target)
  }

  def getResult(starting : List[Int], howMany : Int) = {
    val startingMap : Map[Int, Int] = starting.zipWithIndex.reverse.tail.map(a => a._1 -> (a._2 + 1)).toMap
    generate(starting.last, startingMap, starting.size + 1, howMany)
  }

  def findAnswer1(starting : List[Int]) = {
    getResult(starting, 2020)
  }

  def findAnswer2(starting : List[Int]) = {
    getResult(starting, 30000000)
  }


  println(findAnswer1(List(1,2,16,19,18,0)))
  println(findAnswer2(List(1,2,16,19,18,0)))
}

