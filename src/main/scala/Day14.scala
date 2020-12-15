case class UnsignedIntThirtySix(value : Long)  {

  def internalAsList(current : Long) : List[Char] = {
    val thisOne = current % 2
    thisOne.toString.head :: ( if (current > 1) internalAsList((current - thisOne) / 2) else List.empty)
  }
  def asList : List[Char] = internalAsList(value).padTo(36, '0')
}

object UnsignedIntThirtySix {
  def internalFromList(current : Long, remaining : List[Char], total : Long): Long = {
    remaining match {
      case Nil => total
      case x :: xs if x == '1' => internalFromList(current * 2, xs, total + current)
      case _ :: xs => internalFromList(current * 2, xs, total)
    }
  }
  def fromList(list : List[Char]) : UnsignedIntThirtySix = UnsignedIntThirtySix(internalFromList(1, list, 0))
}

case class Mask(bits : List[Char]) {
  def applyMask(number : UnsignedIntThirtySix): UnsignedIntThirtySix = {

    val a  = bits.zip(number.asList).map( _ match {
      case ('X', x) => x
      case ('1', _) => '1'
      case ('0', _) => '0'
    })
    UnsignedIntThirtySix.fromList(a)
  }

  def applyWithFloatingMask(number : UnsignedIntThirtySix): List[UnsignedIntThirtySix] = {
    def applyWithFloatInternal(remaining : List[(Char, Char)]) : List[List[Char]] = {
      remaining match {
        case Nil => List(List.empty)
        case x :: xs => x match {
          case ('X', _) => List('0','1').flatMap(a => applyWithFloatInternal(xs).map(a :: _))
          case ('1', _) => applyWithFloatInternal(xs).map('1' :: _)
          case ('0', z) => applyWithFloatInternal(xs).map(z :: _)
        }
      }
    }

    val a  = bits.zip(number.asList)
    applyWithFloatInternal(a).map(UnsignedIntThirtySix.fromList)
  }
}

case class State(currentMask : Option[Mask], currentMemory : Map[UnsignedIntThirtySix, UnsignedIntThirtySix]) {
  def getValue = currentMemory.toList.map(_._2.value).sum
}

object Day14 extends App {

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day14TestData"
  val input = scala.io.Source.fromFile(fileName).getLines().toList

  def processLine(currentState : State, line : String) : State = {
    if (line.take(4) == "mask") {
      val newMask =Mask(line.drop(7).toList.reverse)
      currentState.copy(currentMask = Some(newMask))
    } else {
      val address = UnsignedIntThirtySix((line.drop(4).takeWhile(_ != ']')).toLong)
      val value  = currentState.currentMask.map(_.applyMask(UnsignedIntThirtySix(line.drop(4).dropWhile(_ != ']').drop(4).toLong))).get
      currentState.copy(currentMemory = currentState.currentMemory + (address -> value))
    }
  }

  def processLine2(currentState : State, line : String) : State = {
    if (line.take(4) == "mask") {
      val newMask =Mask(line.drop(7).toList.reverse)
      currentState.copy(currentMask = Some(newMask))
    } else {
      val addresses : List[UnsignedIntThirtySix] = currentState.currentMask.map(_.applyWithFloatingMask(UnsignedIntThirtySix((line.drop(4).takeWhile(_ != ']')).toLong))).get
      val value  = UnsignedIntThirtySix(line.drop(4).dropWhile(_ != ']').drop(4).toLong)
      currentState.copy(currentMemory = currentState.currentMemory ++ addresses.map(address => (address -> value)))
    }
  }

  def findAnswer1(input : List[String]) = {
    val endState = input.foldLeft(State(None, Map.empty))(processLine)

    endState.currentMemory.toList.map(_._2.value).sum
  }

  def findAnswer2(input : List[String]) = {
    val endState = input.foldLeft(State(None, Map.empty))(processLine2)

    endState.currentMemory.toList.map(_._2.value).sum
  }

  println(findAnswer1(input))
  println(findAnswer2(input))
}

