import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day9Spec extends AnyFlatSpec {

  val exampleIn = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576".split('\n').toList.map(_.toLong)

  "example" should "give correct result" in {
     Day9.findAnswer1(5,exampleIn) shouldBe 127
  }

  "example" should "give correct result2" in {
    Day9.findAnswer2(5,exampleIn) shouldBe 62
  }


}