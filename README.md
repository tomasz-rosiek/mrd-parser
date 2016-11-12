[![Build Status](https://travis-ci.org/tomasz-rosiek/mrd-parser.svg?branch=master)](https://travis-ci.org/tomasz-rosiek/mrd-parser)

Typesafe parser of the data encoded on machine readable passport.

The format of data available on machine readable travel documents is
available on Wikipedia. (https://en.wikipedia.org/wiki/Machine-readable_passport)

The library allows to define different formats of documents encoded 
using ICAO 9303 standard (passports/identity cards/others) and to define
parsers that parse these documents into case classes in typesafe manner.

Sample code:

    val sampleData = """
    P<UTOERIKSSON<<ANNA<MARIA<<<<<<<<<<<<<<<<<<<
    L898902C<3UTO6908061F9406236ZE184226B<<<<<14
    """
    
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
    
    val documentFormat = Decoder.documentFormat[PassportMetadata](
        StrField("P", 1) ::
        StrField("Type", 1) ::
        StrField("Country", 3) ::
        StrField("Surname", 39) ::
        StrField("Passport number", 10) ::
        StrField("Nationality", 3) ::
        DateField("Date of birth") ::
        SexField("Sex") ::
        DateField("Expiration date") ::
        StrField("Personal number", 15) ::
        StrField("Checksum", 1) :: HNil
    )
    
    println(documentFormat.decode(sampleData))
