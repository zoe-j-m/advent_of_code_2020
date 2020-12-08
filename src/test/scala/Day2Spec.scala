import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day2Spec extends AnyFlatSpec {
  "password" should "be valid if it contains the right number of chars" in {
    Password(1,2,'a', List('a','n')).valid shouldBe true
  }

  "fromString" should "create the correct result" in {
    Password.fromString("1-2 a: aaab") shouldBe Some(Password(1,2,'a', List('a','a', 'a', 'b')))
  }

  "password" should "be valid2 if it contains the right chars in right positions chars" in {
    Password(1,3,'a', List('a','n', 'b')).valid2 shouldBe true
    Password(1,3,'a', List('b','n', 'b')).valid2 shouldBe false
    Password(1,3,'a', List('a','n', 'a')).valid2 shouldBe false
  }

}