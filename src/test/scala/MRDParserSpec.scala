import java.time.LocalDate

import org.scalatest.{GivenWhenThen, FunSuite, Matchers}
import pl.tzr.passport._
import pl.tzr.passport.MRDParser.{GenericDecoder}
import pl.tzr.passport.{SexField, DateField, StrField, Sex}
import shapeless.HNil

class MRDParserSpec extends FunSuite with Matchers with GivenWhenThen {

  case class PassportMetadata(
                               documentType: String,
                               documentSubtype: String,
                               country: String,
                               surname: String,
                               passportNumber: String,
                               nationality: String,
                               dateOfBirth: LocalDate,
                               sex: Sex,
                               expirationDate: LocalDate,
                               personalNumber: String,
                               checksum: String)

  case class IdentityCardMetadata(
                                   documentType: String,
                                   documentSubtype: String,
                                   country: String,
                                   passportNumber: String,
                                   reserved1 : String,
                                   dateOfBirth: LocalDate,
                                   sex: Sex,
                                   expirationDate: LocalDate,
                                   nationality: String,
                                   reserved2 : String,
                                   checksum: String,
                                   surname: String
                                 )

  val passportFormat = MRDParser.documentFormat[PassportMetadata](
    StrField("DocumentType", 1) ::
      StrField("DocumentSubtype", 1) ::
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

  val identityCardFormat = MRDParser.documentFormat[IdentityCardMetadata](
    StrField("DocumentType", 1) ::
    StrField("DocumentSubtype", 1) ::
    StrField("Country", 3) ::
    StrField("Document number", 10, checksum = false) ::
    StrField("Reserved1", 15) ::
    DateField("Date of birth") ::
    SexField("Sex") ::
    DateField("Expiration date") ::
    StrField("Nationality", 3) ::
    StrField("Reserved2", 11) ::
    StrField("Checksum", 1) ::
    StrField("Surname", 30) :: HNil
  )

  test("It should be possible to parse valid passport") {

    Given("we have valid passport data")

    val sampleData = """
  P<UTOERIKSSON<<ANNA<MARIA<<<<<<<<<<<<<<<<<<<
  L898902C<3UTO6908061F9406236ZE184226B<<<<<14
                     """

    When("we parse valid the data")

    val parsedData = passportFormat.decode(sampleData)

    Then("we should get proper content")

    parsedData should be (Right(PassportMetadata("P","","UTO","ERIKSSON  ANNA MARIA","L898902C","UTO",LocalDate
    .parse("1969-08-06"),Female, LocalDate.parse("1994-06-23"), "ZE184226B","4")))


  }

  test("It should be possible to parse valid identity card") {

    Given("we have valid identity card data")

    val sampleData =
      """
        I<POLABA1234568<<<<<<<<<<<<<<<
        8001232F1201239POL<<<<<<<<<<<4
        CURUS<BACHLEDA<<ANNA<MARIA<<<<
      """.stripMargin

    When("we parse valid data")

    val parsedData = identityCardFormat.decode(sampleData)

    Then("we should get proper content")

    parsedData should be (Right(IdentityCardMetadata("I","","POL","ABA1234568","",LocalDate
      .parse("1980-01-23"),Female, LocalDate.parse("2012-01-23"), "POL", "","4", "CURUS BACHLEDA  ANNA MARIA")))
  }

  test("Parser should fail if passport do not match expected format") {
    //TODO
  }

  test("Parser should fail if some of field checksums are not correct") {
    Given("we have valid passport data")

    val sampleData = """
  P<UTOERIKSSON<<ANNA<MARIA<<<<<<<<<<<<<<<<<<<
  L898902C<3UTO6908066F9406236ZE184226B<<<<<84
                     """

    When("we parse valid the data")

    val parsedData = passportFormat.decode(sampleData)

    Then("we should get proper list of errors")

    parsedData should be (Left(List(
      ValidationError("Date of birth", "Invalid checksum"),
      ValidationError("Personal number", "Invalid checksum"))))
  }

  test("Parser should fail for invalid dates") {
    Given("we have valid passport data")

    val sampleData = """
  P<UTOERIKSSON<<ANNA<MARIA<<<<<<<<<<<<<<<<<<<
  L898902C<3UTO6908X61F9406236ZE184226B<<<<<14
                     """

    When("we parse valid the data")

    val parsedData = passportFormat.decode(sampleData)

    Then("we should get a notification about invalid date")

    parsedData should be (Left(List(
      ValidationError("Date of birth", "Invalid date"))))

  }

  test("Parser should fail for invalid values for sex field") {
    Given("we have valid passport data")

    val sampleData = """
  P<UTOERIKSSON<<ANNA<MARIA<<<<<<<<<<<<<<<<<<<
  L898902C<3UTO6908061X9406236ZE184226B<<<<<14
                     """

    When("we parse valid the data")

    val parsedData = passportFormat.decode(sampleData)

    Then("we should get a notification about invalid sex")

    parsedData should be (Left(List(ValidationError("Sex", "Invalid value"))))

  }

  test("Parser should fail if global checksum field is not correct") {
    //TODO
  }


}
