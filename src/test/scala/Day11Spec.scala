import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day11Spec extends AnyFlatSpec {

  val emptyLayout = "...\n...\n...".split('\n').toArray
  val fullUnoccupiedLayout = "LLL\nLLL\nLLL".split('\n').toArray
  val fullOccupiedLayout = "###\n###\n###".split('\n').toArray
  val secondPermutationLayout = "\"#L#\nLLL\\n#L#".split('\n').toArray

  val simpleSecondExample = ".............\n.L.L.#.#.#.#.\n.............".split('\n').toArray

  val givenExample = "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL".split('\n').toArray

  "layout from Input of emptyLayout" should "give the correct layout back" in {
    implicit val mode = LayoutMode(4, 0)
    val expectedLayout = Layout(
      Array(
        Array(Floor,Floor,Floor),
        Array(Floor,Floor,Floor),
        Array(Floor,Floor,Floor),
      )
    )
    Layout.fromInput(emptyLayout).sameAs(expectedLayout) shouldBe (true)
  }

  "layout from Input of full layout" should "give the correct layout back" in {
    implicit val mode = LayoutMode(4, 0)
    val expectedLayout = Layout(
      Array(
        Array(Seat(false),Seat(false),Seat(false)),
        Array(Seat(false),Seat(false),Seat(false)),
        Array(Seat(false),Seat(false),Seat(false)),
      )
    )
    Layout.fromInput(fullUnoccupiedLayout).sameAs(expectedLayout) shouldBe (true)
  }

  "permuting an unoccupied layout" should "give the correct layout back" in {
    implicit val mode = LayoutMode(4, 0)
    val expectedLayout = Layout.fromInput(fullOccupiedLayout)
    Layout.fromInput(fullUnoccupiedLayout).permuteIteration.sameAs(expectedLayout) shouldBe (true)
  }

  "permuting given example until stable" should "give the correct answer" in {
    implicit val mode = LayoutMode(4, 0)
    Day11.permuteUntilStable(Layout.fromInput(givenExample), -1) shouldBe 37
  }

  "viewable Seats from 1,1" should "be correct" in {
    implicit val mode = LayoutMode(5, 1)
    Layout.fromInput(simpleSecondExample).getOccupiedViewableSeatsToThreshold(1,1,1) shouldBe (false)
    Layout.fromInput(simpleSecondExample).getOccupiedViewableSeatsToThreshold(1,3,1) shouldBe (true)
  }

  "permuting given example until stable" should "give the correct answer 2" in {
    implicit val mode = LayoutMode(5, 1)
    Day11.permuteUntilStable(Layout.fromInput(givenExample), -1) shouldBe 26
  }


}