case class Password(
                     first : Int,
                     second : Int,
                     char : Char,
                     password : List[Char]
                   ) {
  def valid : Boolean = {
    val count = password.count(_ == char)
    (count >= first) && (count <= second)
  }

  def valid2 : Boolean = {
    if ((first > password.size) || (second > password.size)) false
    else
      {
        val firstCorrect = password(first -1) == char
        val secondCorrect = password(second -1)== char
        (firstCorrect || secondCorrect) && (!firstCorrect || !secondCorrect)
      }
  }
}

object Password {
  def fromString(string : String) : Option[Password] ={
    val regex = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]*)".r
    regex.findFirstMatchIn(string).map(
      found => Password(found.group(1).toInt,found.group(2).toInt,found.group(3).head, found.group(4).toList)
    )
  }
}

object Day2 extends App {

  def findAnswer1(passwords: List[Password]) =
    passwords.count(_.valid)

  def findAnswer2(passwords: List[Password]) =
    passwords.count(_.valid2)

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day2TestData"
  val list = scala.io.Source.fromFile(fileName).getLines().toList.flatMap(Password.fromString)
  println(findAnswer1(list))
  println(findAnswer2(list))

}
