package pl.tzr.passport

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import shapeless._

import scala.util.{Failure, Success, Try}

object MRDParser {

  trait Decoder[F, R] {
    def parse(input : String, f : F) : (Either[List[ValidationError], R], String)
  }

  private def fieldParser[F, R](length : F => Int)(decoder : (F, String) => Either[List[ValidationError], R]) =
    new Decoder[F, R] {

      override def parse(input : String, f : F) : (Either[List[ValidationError], R], String) = {
        val (value, rest) = input.splitAt(length(f))
        (decoder(f, value), rest)
      }
  }

  implicit val stringFieldDecoder = fieldParser[StrField, String](f => f.length) {
    (f : StrField, i : String) =>
      val textWithChecksum = i.replaceAll("<", " ")
      if (f.checksum) {
        val result = textWithChecksum.substring(0, textWithChecksum.length - 2)
        val checksum = textWithChecksum(textWithChecksum.length - 1)
        if (!validateChecksum(result, checksum)) {
          Left(List(ValidationError(f.name, "Invalid checksum")))
        } else {
          Right(result.trim)
        }
      } else {
        Right(textWithChecksum.trim)
      }
  }

  implicit val dateFieldDecoder = fieldParser[DateField, LocalDate](_ => 7) {
    (f : DateField, i : String) =>
    val dateStr = i.substring(0, 6)
      Try { LocalDate.parse(dateStr, DateTimeFormatter.ofPattern("yyMMdd"))} match {
        case Success(date) =>
          if (!validateChecksum(dateStr, i(6))) {
            Left(List(ValidationError(f.name, "Invalid checksum")))
          } else {
            Right(if (date.getYear > 2050) date.minusYears(100) else date)
          }
        case Failure(_) => Left(List(ValidationError(f.name, "Invalid date")))
      }
  }

  implicit val sexFieldDecoder = fieldParser[SexField, Sex](_ => 1) {
    case (_, "M") => Right(Male)
    case (_, "F") => Right(Female)
    case (f, other) => Left(List(ValidationError(f.name, "Invalid value")))
  }

  implicit val hnilDecoder = new Decoder[HNil, HNil] {
    override def parse(input: String, f: HNil) = (Right(HNil), input) //TODO Check for termination
  }

  implicit def hListDecoder[H, T <: HList, RH, RT <: HList](implicit headDecoder : Decoder[H, RH], tailDecoder : Decoder[T, RT])
  = new Decoder[H :: T, RH :: RT] {
    override def parse(input: String, f: H :: T): (Either[List[ValidationError], RH :: RT], String) = {
      val (headField, headRest) = headDecoder.parse(input, f.head)
      val (tailFields, tailRest) = tailDecoder.parse(headRest, f.tail)
      (headField, tailFields) match {
        case (Right(h), Right(t)) => (Right(h :: t), tailRest)
        case (Left(h), Left(t)) => (Left(h ++ t), tailRest)
        case (Left(h), _) => (Left(h), tailRest)
        case (_, Left(t)) => (Left(t), tailRest)
      }

    }
  }

  private def validateChecksum(text : String, checksum : Char) : Boolean = {

    def mapChar(input : Char) : Int = {
      input match {
        case x if x >= 'A' && x <= 'Z' => x - 'A' + 10
        case x if x >= '0' && x <= '9' => x - '0'
        case ' ' => 0
      }
    }

    val generatedChecksum = text.
      map(mapChar).
      zip(Stream.continually(Seq(7, 3, 1).toStream).flatten).
      map(x => x._1 * x._2).
      sum % 10
    generatedChecksum == mapChar(checksum)
  }

  private def decode[I <: HList, R](definition : I, input : String)(implicit decoder : Decoder[I, R]) :
  Either[List[ValidationError], R] =
    decoder.parse(input.filterNot(_.isWhitespace), definition)._1

  implicit class GenericDecoder[R, I, N <: HList](d : DocumentDefinition.Aux[N, R])(implicit gg : Generic.Aux[R, I],
                                                                           decoder : Decoder[N, I]) {
    def decode(input : String): Either[List[ValidationError], R] = {
      val generic = MRDParser.decode[N, I](d.fields, input)
      generic match {
        case Right(v) => Right(gg.from(v))
        case Left(v) => Left(v)
      }
    }

  }

  def documentFormat[T0] = new {

    def apply[F <: HList](fields : F) = {
      new DocumentDefinition(fields) {
        type T = T0
      }
    }

  }

}
