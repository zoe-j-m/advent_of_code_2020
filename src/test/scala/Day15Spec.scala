import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day15Spec extends AnyFlatSpec {

  "findanswer1" should "give the correct result for the example" in {
    Day15.findAnswer1(List(0,3,6)) shouldBe 436
  }

  "findanswer2" should "give the correct result for the example" in {
    Day15.findAnswer2(List(0,3,6)) shouldBe 175594
  }


}