import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day4Spec extends AnyFlatSpec {

  val exampleIn = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in".split("\n")


  "fromStrings" should "have four passports" in {
     Day4.splitUpInput(exampleIn.toList).map(Passport.fromStrings).size shouldBe 4
  }

  "valid" should "have 2 valid passports" in {
    val a = Day4.requiredForQ1
    Day4.splitUpInput(exampleIn.toList).map(Passport.fromStrings).count(_.isValid(Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"))) shouldBe 2
  }

}