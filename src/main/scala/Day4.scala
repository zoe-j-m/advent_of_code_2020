import scala.annotation.tailrec

case class Passport(fields : Map[String, String])
{
  def isValid(requiredFields : Set[String]) = requiredFields.subsetOf(fields.keySet)

  def isValid2(requiredFields : Set[String], validationMap : Map[String, String => Boolean]) = isValid(requiredFields) &&
    fields.forall(
      kv => validationMap.get(kv._1).exists(test => test(kv._2))
    )
}

object Passport{
  def fromStrings(strings: List[String]) = {
    val fieldStrings = strings.mkString(" ").split(' ').toList
    val fieldMap = fieldStrings.map(_.split(':')).map(a => a(0) -> a(1)).toMap
    Passport(fieldMap)
  }
}

object Day4 extends App {
  def requiredForQ1 = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def checkNum(min : Int, max : Int)(str : String)  : Boolean = {
    val int = str.toIntOption.getOrElse(-1)
    (int >= min) && (int <= max)
  }

  def validPid(passId : String) = (passId.length == 9) && passId.toIntOption.isDefined

  def validHcl(hcl : String) = {
    val hclRegex = "^#([a-g]|[0-9]){6}$"
    hcl.matches(hclRegex)
  }

  def a = (_ : String) => true

  def validEcl(ecl: String) = List("amb","blu", "brn","gry","grn", "hzl", "oth").contains(ecl)

  def validHgt(hgt : String) = {
    val units = hgt.drop(hgt.length - 2)
    val value = hgt.take(hgt.length - 2)
    if (units == "cm") checkNum(140,193)(value)
    else if (units == "in") checkNum(59,76)(value)
    else false
  }
  def validationMap = Map[String, String => Boolean](
   "byr" -> checkNum(1920, 2002),
   "iyr" -> checkNum(2010, 2020),
   "eyr" -> checkNum(2020, 2030),
   "ecl" -> validEcl,
   "pid" -> validPid,
   "hcl" -> validHcl,
   "hgt" -> validHgt,
    "cid" -> a
  )

  def findAnswer1(passports: List[Passport]) = passports.count(_.isValid(requiredForQ1))

  def findAnswer2(passports: List[Passport]) = passports.count(_.isValid2(requiredForQ1, validationMap))

  def splitUpInput(lines : List[String]) : List[List[String]] = {
    lines match {
      case Nil => Nil
      case _ => lines.takeWhile(_ != "" ) :: splitUpInput(lines.dropWhile(_ != "").drop(1))
    }
  }

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day4TestData"
  val lines = scala.io.Source.fromFile(fileName).getLines().toList
  val passports = splitUpInput(lines).map(Passport.fromStrings)
  println(findAnswer1(passports))

  println(findAnswer2(passports))
}
