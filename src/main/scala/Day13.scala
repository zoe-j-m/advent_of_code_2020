import scala.util.{Success, Try}

case class Bus(id : BigInt, index : BigInt) {
  def isThisRight(t : BigInt) : Boolean = (t % id + index) % id== 0
}

object Day13 extends App {

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day13TestData"
  val input = scala.io.Source.fromFile(fileName).getLines().toList

  val target = BigInt(input.head.toInt)
  val busses = input(1).split(',').toList

  def getFirstBus(target : BigInt, busses : List[String]) : BigInt = {
    val idAndDiff = busses.filterNot(_ == "x").map(_.toInt).map(id => (id, (id - target % id) % id)).minBy(_._2)
    idAndDiff._1 * idAndDiff._2
  }


  def chineseRemainder(busses: List[Bus]): Option[BigInt] = {

    val prod = busses.map(_.id).product

    def iter(innerBusses: List[Bus], sm: BigInt): BigInt = {
      def mulInv(a: BigInt, b: BigInt): BigInt = {
        def loop(a: BigInt, b: BigInt, x0: BigInt, x1: BigInt): BigInt = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (innerBusses.nonEmpty) {
        val p = prod / innerBusses.head.id

        iter(innerBusses.tail, sm + innerBusses.head.index * mulInv(p, innerBusses.head.id) * p)
      } else sm
    }

    Try {
      iter(busses, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }

  def gcd(a: BigInt, b: BigInt):BigInt=if (b==0) a.abs else gcd(b, a%b)
  def lcm(list: Seq[BigInt]):BigInt=list.foldLeft(BigInt(1))((a, b) => (a/gcd(a,b))*b)

  def findAnswer1(target : BigInt, bussesInput : List[String]): BigInt = {
    getFirstBus(target, bussesInput)
  }

  def findAnswer2(bussesInput : List[String]): BigInt = {
    val busses = bussesInput.zipWithIndex.collect {
      case (id, index) if id != "x" => Bus(id.toInt, index)
    }
   chineseRemainder(busses.map(b => Bus(b.id, b.id - b.index))).getOrElse(BigInt(-1))

  }

  println(findAnswer1(target, busses))
  println(findAnswer2(busses))

  /*
    t = n1 * bid1
    t + bx2 = n2 * bid2
    t = n2 * bid2 - bx2
   */
}
