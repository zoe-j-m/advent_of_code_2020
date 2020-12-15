import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day13Spec extends AnyFlatSpec {

  val givenTarget = 939

  val givenBusses = "7,13,x,x,59,x,31,19".split(',').toList

  "findAnswer1" should "give the correct result for the given example" in {
    Day13.findAnswer1(givenTarget, givenBusses) shouldBe 295
  }

  "findAnswer2" should "give the correct result for the given example" in {
    Day13.findAnswer2(givenBusses) shouldBe 1068781
  }



}