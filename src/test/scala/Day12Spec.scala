import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day12Spec extends AnyFlatSpec {

  val givenExample = "F10\nN3\nF7\nR90\nF11".split('\n').toList.map(Instruction.fromInput)

  "findAnswer1" should "give the correct result for the given example" in {
    Day12.findAnswer1(givenExample) shouldBe 25L
  }

  "findAnswer2" should "give the correct result for the given example" in {
    Day12.findAnswer2(givenExample) shouldBe 286L
  }



}