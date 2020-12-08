import scala.annotation.tailrec

case class ContainedBag(bagDescription: String, number : Int)

case class Bag(description: String, containsBags : List[ContainedBag]) {
  override def toString = {
    val containedDesc = if (containsBags.isEmpty) "no other bags" else containsBags.map(a => if (a.number == 1) s"${a.number} ${a.bagDescription} bag" else s"${a.number} ${a.bagDescription} bags").mkString(", ")
    description + " bags contain " + containedDesc + "."
  }
}

object Bag {
  def fromString(input : String) : Option[Bag] = {
    val overallRegex = "([a-z]* [a-z]*) bags contain ((?:\\w|,| )*)".r
    val individualBagRegex = "(\\d*) (\\w* \\w*) bag(?:s)?".r
    overallRegex.findFirstMatchIn(input).map(
      found => {
        val desc = found.group(1)
        val subBagString = found.group(2)
        val subBags = individualBagRegex.findAllMatchIn(subBagString).toList.map(
          bagMatch => ContainedBag(bagMatch.group(2), bagMatch.group(1).toInt)
        )
        Bag(desc, subBags)
      }
    )
  }
}

object Day7 extends App {
  def getBags(input : List[String]) : Map[String, Bag] = input.flatMap(line => {
    val maybeBag = Bag.fromString(line)
    maybeBag.map(bag => bag.description -> bag)
  }).toMap

  @tailrec
  def findContains(descriptionsToSearchFor : List[String], bagsSearched : Set[String], bagMap : Map[String, Bag]): Set[String] = {
    val bagsThatContain = bagMap.filter(descAndBag => descAndBag._2.containsBags.exists(containedBag => descriptionsToSearchFor.contains(containedBag.bagDescription))).keys.toList
    val newBagsToSearchFor = bagsThatContain.filterNot(bagsSearched.contains)
    newBagsToSearchFor match {
      case Nil => bagsSearched ++ descriptionsToSearchFor.toSet
      case _ => findContains(newBagsToSearchFor, bagsSearched ++ descriptionsToSearchFor, bagMap)
    }
  }

  def generateTotals(bagDesc : String, bagMap : Map[String, Bag], bagSizes: Map[String, Int]): (Int, Map[String,Int]) = {
    val bag = bagMap(bagDesc)
    val (subBagTotal, returnBagSizes) = bag.containsBags.foldLeft((0, bagSizes))(
      (a,b) =>{
        val (bagTotal, nextBagSizes) = getBagTotal(b.bagDescription, a._2, bagMap)
        (a._1 + b.number *bagTotal, nextBagSizes)
      }
    )
    val thisBagTotal = 1 + subBagTotal
    (thisBagTotal, returnBagSizes + (bagDesc -> thisBagTotal))
  }

  def getBagTotal(bagDesc :String, bagSizes : Map[String, Int], bagMap: Map[String, Bag]):(Int, Map[String, Int]) =
    {
      bagSizes.get(bagDesc).map((_, bagSizes)).getOrElse(
        generateTotals(bagDesc, bagMap, bagSizes))
    }

  @tailrec
  def getBagTotals(bags: List[Bag], bagMap: Map[String,Bag], bagSizes: Map[String, Int]) : Map[String, Int] = {
    bags match {
      case Nil => bagSizes
      case bag::moreBags  => {
        val (_, newBagSizes) = getBagTotal(bag.description, bagSizes, bagMap)
        getBagTotals(moreBags, bagMap, newBagSizes)
      }
    }
  }

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day7TestData"
  val lines = scala.io.Source.fromFile(fileName).getLines().toList

  val bags = getBags(lines)

  def findAnswer1(bagMap : Map[String, Bag]) : Int = {
    val containFound = findContains(List("shiny gold"), Set.empty, bagMap)
    containFound.size - 1 // not this bag itself
  }

  def findAnswer2(bagMap: Map[String, Bag]) : Int = {
    val bagTotals = getBagTotals(bags.values.toList, bags, Map.empty)
    bagTotals.getOrElse("shiny gold", -1) - 1 // not this bag itself
  }

  println(findAnswer1(bags))
  println(findAnswer2(bags))

}
