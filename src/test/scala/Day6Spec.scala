import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day6Spec extends AnyFlatSpec {

  val exampleIn = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"

  "findAnswer1" should "give the correct answer" in {
    Day6.findAnswer1(Day6.splitUpInput(exampleIn.split('\n').toList, List.empty)) should === (11)
  }


  "findAnswer2" should "give the correct answer" in {
    Day6.findAnswer2(Day6.splitUpInput(exampleIn.split('\n').toList, List.empty)) should === (6)
  }

}