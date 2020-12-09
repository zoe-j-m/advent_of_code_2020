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

  def initialState = ComputerState(0,0)

  def replaceOperation(index : Int, operation: Operation, program : List[Operation]) : List[Operation] = {
    program.take(index) ++ (operation :: program.drop(index + 1))
  }

  def switchAroo(index : Int, program : List[Operation]) : Option[List[Operation]] = {
    program(index) match {
      case Jmp(value) => Some(replaceOperation(index, NoOp(value), program))
      case NoOp(value) => Some(replaceOperation(index, Jmp(value), program))
      case _ => None
    }
  }

  def executeUntilEnds(computerState: ComputerState, program : List[Operation], executedInstructions : Set[Int]) : Result =
    if
      (executedInstructions.contains(computerState.programCounter)) InfiniteLoop(computerState.accumulator)
    else
      if
        (computerState.programCounter >= program.size) Terminates(computerState.accumulator)
      else
        executeUntilEnds(program(computerState.programCounter).perform(computerState), program, executedInstructions + computerState.programCounter)

  def checkCandidates(candidates : List[List[Operation]]) : Option[Result] = {
    candidates match {
      case Nil => None
      case program :: remainingCandidates => {
        val executedResult = executeUntilEnds(initialState, program, Set.empty)
        executedResult match {
          case Terminates(_) => Some(executedResult)
          case _ => checkCandidates(remainingCandidates)
        }
      }
    }
  }

  def findAnswer1(program : List[Operation]) : Int = {
    executeUntilEnds(initialState, program, Set.empty).accumulatorValue
  }

  def findAnswer2(program : List[Operation]) : Int = {
    val candidates = program.indices.flatMap(x => switchAroo(x, program)).toList
    checkCandidates(candidates).map(_.accumulatorValue).getOrElse(-1)
  }

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day8TestData"
  val lines = scala.io.Source.fromFile(fileName).getLines().toList
  val program = lines.flatMap(Operation.fromString)

  println(findAnswer1(program))
  println(findAnswer2(program))

}
