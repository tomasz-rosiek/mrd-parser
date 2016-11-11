import java.time.LocalDate

import org.scalatest.{GivenWhenThen, FunSuite, Matchers}
import pl.tzr.passport._
import MRDParser.GenericDecoder
import pl.tzr.passport.{SexField, DateField, StrField, Sex}
import shapeless.HNil

class MRDParserSpec extends FunSuite with Matchers with GivenWhenThen {

  case class PassportMetadata(
                               p: String,
                               docType: String,
                               country: String,
                               surname: String,
                               passportNumber: String,
                               nationality: String,
                               dateOfBirth: LocalDate,
                               sex: Sex,
                               expirationDate: LocalDate,
                               personalNumber: String,
                               checksum: String)

  val documentFormat = MRDParser.documentFormat[PassportMetadata](
    StrField("P", 1) ::
      StrField("Type", 1) ::
      StrField("Country", 3) ::
      StrField("Surname", 39) ::
      StrField("Passport number", 10, checksum = true) ::
      StrField("Nationality", 3) ::
      DateField("Date of birth") ::
      SexField("Sex") ::
      DateField("Expiration date") ::
      StrField("Personal number", 15, checksum = true) ::
      StrField("Checksum", 1) :: HNil
  )


  test("It should be possible to parse valid passport") {

    Given("we have valid passport data")

    val sampleData = """
  P<UTOERIKSSON<<ANNA<MARIA<<<<<<<<<<<<<<<<<<<
  L898902C<3UTO6908061F9406236ZE184226B<<<<<14
                     """

    When("we parse valid the data")

    val parsedData = documentFormat.decode(sampleData)

    Then("we should get proper content")

    parsedData should be (PassportMetadata("P","","UTO","ERIKSSON  ANNA MARIA","L898902C","UTO",LocalDate
    .parse("1969-08-06"),Female, LocalDate.parse("1994-06-23"), "ZE184226B","4"))


  }

  test("Parser should fail if passport do not match expected format") {
    //TODO
  }

  test("Parser should fail if one of field checksums are not correct") {
    //TODO
  }

  test("Parser should fail if global checksum field is not correct") {
    //TODO
  }


}
