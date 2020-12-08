import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day1Spec extends AnyFlatSpec {
  "findSumToTwentyTwenty" should "return none if passed empty lists" in {
    Day1.findSumToTwentyTwenty(2, List.empty) should === (None)
  }

  "findSumToTwentyTwenty" should "return the two correct two answers" in {
    val list = List(2000, 1, 20)
    Day1.findSumToTwentyTwenty(2, list) should === (Some(List(2000,20)))
  }

  "findAnswer" should "return the correct answer" in {
    val list = List(2, 2000, 1, 20)
    Day1.findAnswer(2,list) should === (40000)
  }

  979
  366
  299
  675
  1456

  "findAnswer" should "return the correct answer for the given example" in {
    val list = List(  1721,
      979,
    366,
    299,
    675,
    1456)
    Day1.findAnswer(2, list) should === (514579)
  }

}