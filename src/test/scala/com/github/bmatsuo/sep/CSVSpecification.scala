package com.github.bmatsuo.sep

import com.github.tototoshi.csv._
import org.scalacheck.{Shrink, Properties, Gen}
import org.scalacheck.Prop.forAll
import scalaz.NonEmptyList
import java.nio.charset.Charset

object CSVSpecification extends Properties("CSV") {
  import CSV._

  import scala.language.implicitConversions

  implicit def shrinkNel[A](implicit s: Shrink[A]): Shrink[NonEmptyList[A]] = Shrink { nel ⇒
    (for {
      h ← Shrink.shrink(nel.head)
      q ← Shrink.shrink(nel.tail)
    } yield NonEmptyList(h, q:_*)) append
    (for (h ← Shrink.shrink(nel.head)) yield NonEmptyList(h, nel.tail:_*)) append
    (for (q ← Shrink.shrink(nel.tail)) yield NonEmptyList(nel.head, q:_*))
  }

  val allChars =  {
    import java.lang.Character._

    (Char.MinValue to Char.MaxValue).toSeq
      //.filter(isLetterOrDigit)
      .filter(isDefined)
      .filterNot(isISOControl)
      .filterNot(c ⇒ c < 0x20 && c != 0x0A && c != 0x0D)
      .map(_.toString)
  }

  val anyChar =
    Gen.oneOf(allChars)

  val anyStr =
    Gen.listOf(anyChar)
      .map(_.mkString)

  val boundedStr =
    for {
      len   ← Gen.choose(0, 3)
      chars ← Gen.listOfN(len, anyChar)
    } yield chars.mkString

  val row =
    Gen.nonEmptyListOf(boundedStr)
      .map(cells ⇒ NonEmptyList(cells.head, cells.tail:_*))

  val table =
    Gen.nonEmptyListOf(row)
      .map(rows ⇒ NonEmptyList(rows.head, rows.tail:_*))

  val textNonQuoteChar =
    anyChar.suchThat(_ != rfc4180.quote)

  val textQuoteEscapedChar =
    Gen.frequency(
      (1, Gen.const(rfc4180.qesc)),
      (100, textNonQuoteChar))

  val textQuotedString =
    for {
      str    ← Gen.listOf(textQuoteEscapedChar).map(_.mkString)
      quoted = "\"" + str + "\""
    } yield quoted

  val textString =
    Gen.listOf(textNonQuoteChar)
      .map(_.mkString)

  val textCell =
    Gen.frequency(
      (1, textQuotedString),
      (1, textString))

  val textRow =
    Gen.nonEmptyListOf(textCell)
      .map(_.mkString(rfc4180.comma.toString))

  val textTable =
    Gen.nonEmptyListOf(textRow)
      .map(_.mkString("\r\n"))

  /*
  property("stablility") = forAll(table) { (table: Table) ⇒
    val cycle = rfc4180.load(rfc4180.export(table))
    (cycle | empty) == table
  }
  */

  ///*
  property("scala-csv stability") = forAll (table) { (table: Table) ⇒
    val orig = table.map(_.list).list
    val buf = new java.io.StringWriter()

    val w = CSVWriter.open(buf)
    w.writeAll(orig)
    w.close

    val r = CSVReader.open(new java.io.StringReader(buf.toString))
    val t = r.all
    r.close

    t == orig
  }
  //*/
}
