import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day5Spec extends AnyFlatSpec {

  val exampleIn = "FBFBBFFRLR"

  "fromInput" should "give the correct answer" in {
    SeatLocation.fromInput(exampleIn) shouldBe DefiniteSeatLocation(44, 5)
  }

}