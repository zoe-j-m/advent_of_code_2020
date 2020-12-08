import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day3Spec extends AnyFlatSpec {

  val exampleIn = List("..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#")

  "treeCount" should "be right in the example" in {
     MountainSide.fromStrings(exampleIn).treeCount(3,1) shouldBe 7
  }


}