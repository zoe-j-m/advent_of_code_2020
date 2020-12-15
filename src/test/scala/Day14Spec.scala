import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day14Spec extends AnyFlatSpec {

  val example = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0".split('\n').toList

  val example2 = "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1".split('\n').toList
  "asList" should "give the correct result for value 1" in {
     UnsignedIntThirtySix(1).asList.mkString("") shouldBe ("100000000000000000000000000000000000")
  }

  "asList" should "give the correct result for value 551" in {
    UnsignedIntThirtySix(551).asList.mkString("") shouldBe ("111001000100000000000000000000000000")
  }

  "asList the fromList" should "give the correct result for a value" in {
    UnsignedIntThirtySix.fromList(UnsignedIntThirtySix(3231).asList) shouldBe (UnsignedIntThirtySix(3231))
  }

  "findanswer1" should "give the correct result for the example" in {
    Day14.findAnswer1(example) shouldBe 165
  }

  "findanswer2" should "give the correct result for the example" in {
    Day14.findAnswer2(example2) shouldBe 208
  }

}