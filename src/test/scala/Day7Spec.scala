import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day7Spec extends AnyFlatSpec {

  val exampleIn = "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.".split('\n').toList

  "Bag from string" should "give the correct bag back" in {
    val expectedBag = Bag("dim tan", List(ContainedBag("shiny teal",3), ContainedBag("bright white",5), ContainedBag("striped bronze",4)))
    Bag.fromString("dim tan bags contain 3 shiny teal bags, 5 bright white bags, 4 striped bronze bags.") shouldBe Some(expectedBag)
  }

  "Bag from string" should "work each way" in {
    val expectedBag = Bag("dim tan", List(ContainedBag("shiny teal",3), ContainedBag("bright white",5), ContainedBag("striped bronze",4)))
    Bag.fromString("dim tan bags contain 3 shiny teal bags, 5 bright white bags, 4 striped bronze bags.").get.toString shouldBe "dim tan bags contain 3 shiny teal bags, 5 bright white bags, 4 striped bronze bags."
  }

  "Bag from string" should "give the correct bag back when no sub bags" in {
    val expectedBag = Bag("faded blue", List.empty)
    Bag.fromString("faded blue bags contain no other bags.") shouldBe Some(expectedBag)
  }

  "example" should "give correct result" in {
    val bags = Day7.getBags(exampleIn)
    bags.size shouldBe 9
    Day7.findContains(List("shiny gold"), Set.empty, bags).size shouldBe 5
  }

  "example" should "get correct bag totals" in {
    val bags = Day7.getBags(exampleIn)
    val bagTotals = Day7.getBagTotals(bags.values.toList, bags, Map.empty)
    bagTotals.getOrElse("shiny gold", -1) shouldBe 33
  }


}