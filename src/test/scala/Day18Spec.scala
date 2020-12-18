import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day18Spec extends AnyFlatSpec {


  "Evaluating" should "give the correct result for the examples" in {
    implicit val rules = 0
    Expression.fromString("2 * 3 + (4 * 5)").evaluate.value shouldBe 26
    Expression.fromString("5 + (8 * 3 + 9 + 3 * 4 * 3)").evaluate.value shouldBe 437
    Expression.fromString("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))").evaluate.value shouldBe 12240
    Expression.fromString("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").evaluate.value shouldBe 13632
  }

  "Evaluating 2" should "give the correct result for the examples" in {
    implicit val rules = 1
    Expression.fromString("2 * 3 + (4 * 5)").evaluate.value shouldBe 46
    Expression.fromString("5 + (8 * 3 + 9 + 3 * 4 * 3)").evaluate.value shouldBe 1445
    Expression.fromString("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))").evaluate.value shouldBe 669060
    Expression.fromString("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").evaluate.value shouldBe 23340
  }


}

