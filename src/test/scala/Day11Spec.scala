import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day11Spec extends AnyFlatSpec {

  val emptyLayout = "...\n...\n...".split('\n').toList
  val fullUnoccupiedLayout = "LLL\nLLL\nLLL".split('\n').toList
  val fullOccupiedLayout = "###\n###\n###".split('\n').toList
  val secondPermutationLayout = "\"#L#\nLLL\\n#L#".split('\n').toList

  val simpleSecondExample = ".............\n.L.L.#.#.#.#.\n.............".split('\n').toList

  val givenExample = "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL".split('\n').toList

  "layout from Input of emptyLayout" should "give the correct layout back" in {
    implicit val mode = LayoutMode(4, 0)
    val expectedLayout = Layout(
      List(
        List(Floor,Floor,Floor),
        List(Floor,Floor,Floor),
        List(Floor,Floor,Floor),
      )
    )
    Layout.fromInput(emptyLayout) shouldBe (expectedLayout)
  }

  "layout from Input of full layout" should "give the correct layout back" in {
    implicit val mode = LayoutMode(4, 0)
    val expectedLayout = Layout(
      List(
        List(Seat(false),Seat(false),Seat(false)),
        List(Seat(false),Seat(false),Seat(false)),
        List(Seat(false),Seat(false),Seat(false)),
      )
    )
    Layout.fromInput(fullUnoccupiedLayout) shouldBe (expectedLayout)
  }

  "permuting an unoccupied layout" should "give the correct layout back" in {
    implicit val mode = LayoutMode(4, 0)
    val expectedLayout = Layout.fromInput(fullOccupiedLayout)
    Layout.fromInput(fullUnoccupiedLayout).permuteIteration shouldBe (expectedLayout)
  }

  "permuting given example until stable" should "give the correct answer" in {
    implicit val mode = LayoutMode(4, 0)
    Day11.permuteUntilStable(Layout.fromInput(givenExample)) shouldBe 37
  }

  "viewable Seats from 1,1" should "be correct" in {
    implicit val mode = LayoutMode(5, 1)
    Layout.fromInput(simpleSecondExample).getOccupiedViewableSeatsToThreshold(1,1,1) shouldBe (false)
    Layout.fromInput(simpleSecondExample).getOccupiedViewableSeatsToThreshold(1,3,1) shouldBe (true)
  }

  "permuting given example until stable" should "give the correct answer 2" in {
    implicit val mode = LayoutMode(5, 1)
    Day11.permuteUntilStable(Layout.fromInput(givenExample)) shouldBe 26
  }


}