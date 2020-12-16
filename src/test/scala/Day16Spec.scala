import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day16Spec extends AnyFlatSpec {
val example = "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12".split('\n').toList

  "Rule From String" should "give the correct rule for the example" in {
    Rule.fromString("fred astaire: 12-14 or 134-155") shouldBe Some(Rule("fred astaire", List(12 to 14, 134 to 155)))
  }

  "findAnswer1" should "give the correct result for the example" in {
    Day16.findAnswer1(example) shouldBe 71
  }

}