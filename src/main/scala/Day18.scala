sealed trait Term {
  def evaluate(implicit rules: Int) : Value
}
case class Value(value : BigInt) extends Term {
  override def evaluate(implicit rules: Int): Value = this
}
case class Operator(operator : MathOperation) extends Term {
  override def evaluate(implicit rules: Int): Value = throw new Exception("Can't")
}
case class Parenthesis(expression : Expression) extends Term {
  override def evaluate(implicit rules: Int): Value = expression.evaluate
}

sealed trait MathOperation {
  def calculate(value1 : Value, value2 : Value) : Value
}

case object Add extends MathOperation {
  override def calculate(value1: Value, value2: Value): Value = Value(value1.value + value2.value)
}

case object Multiply extends MathOperation {
  override def calculate(value1: Value, value2: Value): Value = Value(value1.value * value2.value)
}

case class Expression(terms : List[Term]) {
  def evaluate(implicit rules : Int) : Value = {
    terms match {
      case x :: Nil => x.evaluate
      case x :: Operator(op) :: y :: xs if rules == 0 || op == Add => Expression(op.calculate(x.evaluate, y.evaluate) :: xs).evaluate
      case x :: Operator(Multiply) :: y :: xs => Multiply.calculate(x.evaluate, Expression(y :: xs).evaluate)
      case _ => throw new Exception("Malformed expression: " + terms)
    }
  }
}

object Expression {
  def fromString(input : String): Expression = {
    def fromListOfChar(chars : List[Char]) : (List[Term], List[Char]) = {
      chars match {
        case Nil => (List.empty, List.empty)
        case x :: _ if x.isDigit => {
          val (terms, remainder) = fromListOfChar(chars.dropWhile(_.isDigit))
          (Value(BigInt(chars.takeWhile(_.isDigit).mkString)) :: terms, remainder)
        }
        case x :: xs if x == '+' => {
          val (terms, remainder) = fromListOfChar(xs)
          (Operator(Add) :: terms, remainder)
        }
        case x :: xs if x == '*' => {
          val (terms, remainder) = fromListOfChar(xs)
          (Operator(Multiply) :: terms, remainder)
        }
        case x :: xs if x == '(' => {
          val (terms, remainder1) = fromListOfChar(xs)
          val (moreTerms, remainder) = fromListOfChar(remainder1)
          (Parenthesis(Expression(terms)) :: moreTerms, remainder)
        }
        case x :: xs if x == ')' => {
          (List.empty, xs)
        }
        case _ => throw new Exception("Unrecognised characters in input")
      }
    }

     Expression(fromListOfChar(input.toList.filterNot(_ == ' '))._1)
  }
}

object Day18 extends App {

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day18TestData"
  val input = scala.io.Source.fromFile(fileName).getLines().toList

  def findAnswer1(input: List[String]) = {
    implicit val rules = 0
    input.map(a => Expression.fromString(a).evaluate.value).sum
  }


  def findAnswer2(input: List[String]) = {
    implicit val rules = 1
    input.map(a => Expression.fromString(a).evaluate.value).sum
  }


  println(findAnswer1(input))
  println(findAnswer2(input))
}

