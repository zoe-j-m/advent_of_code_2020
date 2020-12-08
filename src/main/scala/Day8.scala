import scala.annotation.tailrec

case class ComputerState(programCounter : Integer, accumulator: Integer)

sealed trait Operation {
  def perform(inputState : ComputerState) : ComputerState
}
case class NoOp(redundantNumber : Int) extends Operation {
  override def perform(inputState : ComputerState) : ComputerState = ComputerState(  inputState.programCounter + 1, inputState.accumulator)
}
case class Acc(amount: Int) extends Operation
{
  override def perform(inputState : ComputerState) : ComputerState = ComputerState(  inputState.programCounter + 1, inputState.accumulator + amount)
}
case class Jmp(relative: Int) extends Operation
{
  override def perform(inputState : ComputerState) : ComputerState = ComputerState(  inputState.programCounter + relative, inputState.accumulator)
}

sealed trait Result {
  def accumulatorValue : Int
}
case class Terminates(accumulatorValue : Int) extends Result
case class InfiniteLoop(accumulatorValue : Int) extends Result


object Operation {

  def fromString(input : String) : Option[Operation] = {
    val operationRegex = "(\\w{3}) ((?:\\+|-)\\d*)".r
    operationRegex.findFirstMatchIn(input).map(
      found => {
        found.group(1) match {
          case "nop" => NoOp(found.group(2).toInt)
          case "acc" => Acc(found.group(2).toInt)
          case "jmp" => Jmp(found.group(2).toInt)
        }
      }
    )
  }
}

object Day8 extends App {
  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day8TestData"
  val lines = scala.io.Source.fromFile(fileName).getLines().toList
  val program = lines.flatMap(Operation.fromString)

  def replaceX(x : Int, operation: Operation, program : List[Operation]) : List[Operation] = {
    program.take(x) ++ (operation :: program.drop(x + 1))
  }

  def switchAroo(x : Int, program : List[Operation]) : Option[List[Operation]] = {
    program(x) match {
      case Jmp(value) => Some(replaceX(x, NoOp(value), program))
      case NoOp(value) => Some(replaceX(x, Jmp(value), program))
      case _ => None
    }
  }

  def executeUntilEnds(computerState: ComputerState, program : List[Operation], executedInstructions : Set[Int]) : Result =
    if (executedInstructions.contains(computerState.programCounter)) InfiniteLoop(computerState.accumulator)
    else
      if (computerState.programCounter >= program.size) Terminates(computerState.accumulator)
      else executeUntilEnds(program(computerState.programCounter).perform(computerState), program, executedInstructions + computerState.programCounter)

  def checkOptions(options : List[List[Operation]]) : Option[Result] = {
    options match {
      case Nil => None
      case program :: remainingOptions => {
        val executedResult = executeUntilEnds(ComputerState(0,0), program, Set.empty)
        executedResult match {
          case Terminates(_) => {
            println(program)
            Some(executedResult)
          }
          case _ => checkOptions(remainingOptions)
        }
      }
    }
  }

  def findAnswer1(program : List[Operation]) : Int = {
    executeUntilEnds(ComputerState(0,0), program, Set.empty).accumulatorValue
  }

  def findAnswer2(program : List[Operation]) : Int = {
    val options = program.indices.flatMap(x => switchAroo(x, program)).toList
    checkOptions(options).map(_.accumulatorValue).getOrElse(-1)
  }

  println(findAnswer1(program))
  println(findAnswer2(program))

}
