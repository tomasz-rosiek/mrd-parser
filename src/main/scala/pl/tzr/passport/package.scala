package pl.tzr.passport

import java.time.LocalDate

import shapeless.HList

sealed trait Sex
case object Male extends Sex
case object Female extends Sex

sealed trait DocField {
  type Result
  def name : String
}

case class StrField(name : String, length : Int, checksum : Boolean = false) extends DocField {
  type Result = String
}

case class DateField(name : String) extends DocField {
  type Result = LocalDate
}

case class SexField(name : String) extends DocField {
  type Result = Sex
}

class DocumentDefinition[F <: HList](val fields : F) {
  type T

}

object DocumentDefinition {
  type Aux[F0 <: HList, T0] = DocumentDefinition[F0] {
    type T = T0
  }
}
