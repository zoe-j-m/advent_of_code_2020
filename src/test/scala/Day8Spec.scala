import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day8Spec extends AnyFlatSpec {

  val exampleIn = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6".split('\n').toList

  "Operation from string" should "give the correct operation back" in {
    val expectedOperation = Jmp(-3)
    Operation.fromString("jmp -3") shouldBe Some(expectedOperation)
  }

  "example" should "give correct result" in {
    println(exampleIn)
    val program = exampleIn.flatMap(Operation.fromString)
    program.size shouldBe 9
    Day8.executeUntilEnds(ComputerState(0,0), program, Set.empty).accumulatorValue shouldBe 5
  }

  "example" should "give correct result2" in {
    println(exampleIn)
    val program = exampleIn.flatMap(Operation.fromString)

    Day8.findAnswer2(program) shouldBe 8

  }

}