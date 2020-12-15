import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day10Spec extends AnyFlatSpec {

  val example2 = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3".split('\n').toList.map(_.toLong)

  val example1 = List(1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19).map(_.toLong)

  "findAnswer1" should "give correct result" in {
     Day10.findAnswer1(example2) shouldBe 220
  }

  "findAnswer2" should "give correct result" in {
    Day10.findAnswer2(example1) shouldBe 8
  }

  "breakdown into three gapped" should "give back a list divided into three-gapped segments" in {
    Day10.breakdownIntoThreeGapped(List(0L), example1 ++ List(22L)) shouldBe List(List(0L, 1L), List(4L,5L,6L, 7L), List(10L, 11L,12L), List(15L, 16L), List(19L), List(22L))
  }


  "findAnswer2" should "give correct result2" in {
    Day10.findAnswer2(example2) shouldBe 19208L
  }



}