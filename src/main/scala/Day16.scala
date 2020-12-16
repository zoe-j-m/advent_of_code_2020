
case class Rule(fieldName: String, validRanges : List[Range]) {
  def checkValue(value : Int): Boolean = validRanges.exists(range => range.contains(value))
}

object Rule {
  def fromString(input : String) : Option[Rule] = {
    val regex = "((?:\\w|\\s)*): (\\d*)-(\\d*) or (\\d*)-(\\d*)".r
    regex.findFirstMatchIn(input).map(
      found => Rule(found.group(1), List(found.group(2).toInt to found.group(3).toInt, found.group(4).toInt to found.group(5).toInt))
    )
  }
}

case class Ticket(fields : List[Int]) {
  def invalidValues(rules : Ruleset) : List[Int] = fields.filterNot(rules.exists)
}

object Ticket {
  def fromString(input : String) : Ticket = Ticket(input.split(',').toList.map(_.toInt))
}

case class Ruleset(rules : List[Rule]) {
  def checkRule(valueAndRule : (Int, Rule)) : Boolean = valueAndRule._2.checkValue(valueAndRule._1)
  def checkRules(ticket: Ticket) : Boolean = {
    (ticket.fields.size == rules.size) &&
      ticket.fields.zip(rules).forall(checkRule)
  }
  def exists(value : Int): Boolean = rules.exists(_.checkValue(value))

  def getPossibleSetsForTheseTickets(tickets: List[Ticket]) : Ruleset = {

    def allValuesForFields(fields : List[List[Int]]) : List[List[Int]] =
      if (fields.head.isEmpty) List.empty
      else fields.map(_.head) :: allValuesForFields(fields.map(_.tail))

    def addRuleToPossibleSolutionsIfNotAlreadyThere(currentPossibleSolutions : List[List[(Rule, Int)]], rulesAndIndex : (List[Rule], Int)) = {
      val (availableRules, fieldIndex) = rulesAndIndex
      if
        (currentPossibleSolutions.isEmpty) availableRules.map(c => List((c, fieldIndex)))
      else
        availableRules.flatMap(
          rule => currentPossibleSolutions.collect {
            case possibleSolution if !possibleSolution.map(_._1).contains(rule)  => (rule , fieldIndex) :: possibleSolution
          }
        )
    }

    val allValues = allValuesForFields(tickets.map(_.fields))
    val possibleMatches = allValues.map(fieldset => rules.filter(rule => fieldset.forall(field => rule.checkValue(field)))).zipWithIndex.sortBy(_._1.size)
    val possibleSolutions = possibleMatches.foldLeft[List[List[(Rule, Int)]]](List.empty)(addRuleToPossibleSolutionsIfNotAlreadyThere)
    Ruleset(possibleSolutions.filter(_.size == rules.size).head.sortBy(_._2).map(_._1))
  }
}

object Day16 extends App {

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day16TestData"
  val input = scala.io.Source.fromFile(fileName).getLines().toList

  def fromInput(input : List[String]) = {
    val rules = Ruleset(input.takeWhile(_ != "").flatMap(Rule.fromString))

    val rest = input.dropWhile(_ != "your ticket:")

    val yourTicket = Ticket.fromString(rest.drop(1).head)

    val otherTickets = rest.drop(4).map(Ticket.fromString)

    (rules, yourTicket, otherTickets)
  }


  def findAnswer1(input : List[String]) = {
    val (rules, _, otherTickets) = fromInput(input)
    otherTickets.flatMap(_.invalidValues(rules)).sum
  }

  def findAnswer2(input : List[String]) = {
    val (rules, yourTicket, otherTickets) = fromInput(input)
    val validTickets = otherTickets.filter(_.invalidValues(rules).isEmpty)
    val validRules = rules.getPossibleSetsForTheseTickets(yourTicket :: validTickets)
    validRules.rules.zip(yourTicket.fields).filter(a => a._1.fieldName.startsWith("departure")).map(_._2.toLong).product
  }

  println(findAnswer1(input))
  println(findAnswer2(input))
}

