package pl.tzr.passport

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import shapeless._

object MRDParser {

  trait Decoder[F, R] {
    def parse(input : String, f : F) : (R, String)
  }

  trait FieldDecoder[F, R] extends Decoder[F, R] {
    def parse(input : String, f : F) : (R, String) = {
      val (value, rest) = input.splitAt(length(f))
      (decoder(value, f), rest)
    }
    def length(f : F) : Int
    def decoder(i : String, f : F) : R
  }

  implicit val stringFieldDecoder = new FieldDecoder[StrField, String] {
    override def parse(input : String, f : StrField) : (String, String) = {
      val (value, rest) = input.splitAt(f.length)
      (decoder(value, f), rest)
    }
    override def decoder(i : String, f : StrField) : String = {
      val textWithChecksum = i.replaceAll("<", " ")
      val text = if (f.checksum) {
        val result = textWithChecksum.substring(0, textWithChecksum.length - 2)
        val checksum = textWithChecksum(textWithChecksum.length - 1)
        if (!validateChecksum(result, checksum))
          throw new IllegalStateException("Invalid field")
        result
      } else {
        textWithChecksum
      }
      text.trim
    }

    override def length(f: StrField): Int = f.length

  }

  implicit val dateFieldDecoder = new FieldDecoder[DateField, LocalDate] {

    override def decoder(i : String, f : DateField) = {
      val dateStr = i.substring(0, 6)
      val date = LocalDate.parse(dateStr, DateTimeFormatter.ofPattern("yyMMdd"))
      if (!validateChecksum(dateStr, i(6)))
        throw new IllegalStateException("Invalid date")
      if (date.getYear > 2050) date.minusYears(100) else date
    }

    override def length(f: DateField): Int = 7

  }

  implicit val sexFieldDecoder = new FieldDecoder[SexField, Sex] {

    override def decoder(i : String, f : SexField) = i match {
      case "M" => Male
      case "F" => Female
    }

    override def length(f: SexField): Int = 1
  }

  implicit val hnilDecoder = new Decoder[HNil, HNil] {
    override def parse(input: String, f: HNil): (HNil, String) = (HNil, input) //TODO Check for termination
  }

  implicit def hListDecoder[H, T <: HList, RH, RT <: HList]
  (implicit headDecoder : Decoder[H, RH], tailDecoder : Decoder[T, RT])
  = new Decoder[H :: T, RH :: RT] {
    override def parse(input: String, f: H :: T): (RH :: RT, String) = {
      val headResult = headDecoder.parse(input, f.head)
      val tailResult = tailDecoder.parse(headResult._2, f.tail)
      (headResult._1 :: tailResult._1, tailResult._2)
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

  private def decode[I <: HList, R](definition : I, input : String)(implicit decoder : Decoder[I, R]) : R =
    decoder.parse(input.filterNot(_.isWhitespace), definition)._1

  implicit class GenericDecoder[R, I, N <: HList](d : DocumentDefinition.Aux[N, R])(implicit gg : Generic.Aux[R, I],
                                                                           decoder : Decoder[N, I]) {
    def decode(input : String): R = {
      val generic = MRDParser.decode[N, I](d.fields, input)
      gg.from(generic)
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
