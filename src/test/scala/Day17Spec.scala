import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day17Spec extends AnyFlatSpec {
val example = ".#.\n..#\n###".split('\n').toList.map(_.toList)


  "findAnswer1" should "give the correct result for the example" in {
    Day17.findAnswer1(example) shouldBe 112
  }

  "findAnswer2" should "give the correct result for the example" in {
    Day17pt2.findAnswer2(example) shouldBe 848
  }

}

