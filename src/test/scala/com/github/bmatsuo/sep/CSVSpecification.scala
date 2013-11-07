package com.github.bmatsuo.sep

import org.scalacheck.{Shrink, Properties, Gen}
import org.scalacheck.Prop._
import scalaz.NonEmptyList

object CSVSpecification extends Properties("CSV") {
  import CSV._

  import scala.language.implicitConversions

  implicit def shrinkNel[A](implicit s: Shrink[List[A]]): Shrink[NonEmptyList[A]] = Shrink { nel ⇒
    for {
      lis ← Shrink.shrink(nel.list)
      if !lis.isEmpty
    } yield NonEmptyList(lis.head, lis.tail:_*)
  }

  object GenCommon {
    import Gen._

    val allChars: Seq[Char] = {
      import java.lang.Character._

      for {
        c ← (Char.MinValue to Char.MaxValue).toSeq
        if isDefined(c)
        if !isISOControl(c)
        if !(c < 0x20 && c != 0x0A && c != 0x0D)
      } yield c
    }

    val chars: Gen[Char] =
      oneOf(allChars)

    val safeChars =
      oneOf(allChars filter Character.isLetterOrDigit)

    val stringLengths: Gen[Int] =
      frequency(
        (20, 0),
        (70, 1),
        (85, 3),
        (15, 6),
        (5, 10),
        (1, 70))

    val rowLengths: Gen[Int] =
      frequency(
        (15, 1),
        (70, 2),
        (85, 3),
        (15, 6),
        (5, 20),
        (1, 30))

    val tableLengths: Gen[Int] =
      frequency(
        (10, 1),
        (70, 10),
        (85, 20),
        (15, 30),
        (5, 70),
        (1, 100))
  }

  // Generates a Table with random size (independent in both directions) and random (unicode) string content
  object GenTable {
    import Gen._
    import GenCommon._

    val strings: Gen[String] = for {
      n  ← stringLengths
      cs ← listOfN(n, chars)
    } yield cs.mkString

    val safeStrings: Gen[String] =  for {
      n  ← stringLengths
      cs ← listOfN(n, safeChars)
    } yield cs.mkString

    val rows: Gen[Row] = for {
      n  ← rowLengths
      cs ← listOfN(n, strings)
    } yield NonEmptyList(cs.head, cs.tail:_*)

    val safeRows: Gen[Row] = for {
      n ← rowLengths
      cs ← listOfN(n, safeStrings)
    } yield NonEmptyList(cs.head, cs.tail:_*)

    val tables: Gen[Table] = for {
      n  ← tableLengths
      rs ← listOfN(n, rows)
    } yield NonEmptyList(rs.head, rs.tail:_*)

    val safeTables: Gen[Table] = for {
      n  ← tableLengths
      cs ← listOfN(n, safeRows)
    } yield NonEmptyList(cs.head, cs.tail:_*)
  }

  // Contains generators for CSV documents. The documents are valid according to RFC 4180,
  // with an extention to unicode characters.
  object GenCSVText {
    import GenCommon._
    import Gen._

    val nonQuoteChars: Gen[Char] =
      chars suchThat (_ != rfc4180.quote)

    val quoteEscapedChars: Gen[String] =
      Gen.frequency(
        (1, Gen.const(rfc4180.qesc)),
        (100, nonQuoteChars.map(_.toString)))

    val quotedStrings: Gen[String] = for {
      n  ← stringLengths
      cs ← listOfN(n, quoteEscapedChars)
    } yield cs.mkString("\"", "", "\"")

    val unquotedStrings: Gen[String] = for {
      n  ← stringLengths
      cs ← listOfN(n, nonQuoteChars)
    } yield cs.mkString

    val cells: Gen[String] =
      Gen.frequency(
        (1, quotedStrings),
        (1, unquotedStrings))

    val rows: Gen[String] = for {
      n  ← rowLengths
      cs ← listOfN(n, cells)
    } yield cs.mkString(rfc4180.comma.toString)

    val rowLists: Gen[List[String]] =  for {
      n  ← tableLengths
      cs ← listOfN(n, rows)
    } yield cs

    val rowSeps: Gen[String] = oneOf(Seq("\r\n", "\n"))

    val tables: Gen[String] = for {
      rs  ← rowLists
      sep ← rowSeps
    } yield rs.mkString(sep)
  }

  object Classification {
    val Seconds = 1000L

    val KB = 1024L
    val MB = 1024 * KB

    def sizeCategory(nbytes: Int) =
      nbytes match {
        case _ if nbytes >=  10 * MB ⇒ "XL"
        case _ if nbytes >=   1 * MB ⇒ "L"
        case _ if nbytes >= 100 * KB ⇒ "M"
        case _ if nbytes >=   1 * KB ⇒ "S"
        case _                       ⇒ "XS"
      }

    def numCells(t: Table): Int =
      t.map(_.size).stream.reduce(_ + _)

    def cellsPerByte(t: Table, text: String) =
      numCells(t).toDouble / text.length
  }

  property("load(export(t)) == success(t)") = forAll(GenTable.safeTables) { (table1: Table) ⇒
    import Classification._

    val text = rfc4180.export(table1)
    val table2 = rfc4180.load(text)
    val prop = table2.map(_ == table1).getOrElse(false)

    collect(sizeCategory(text.length))(prop)
  }

  property("rfc4180.load(csv) == success(_) for all valid csv input") = forAll(GenCSVText.tables) { (csv: String) ⇒
    rfc4180.load(csv).isSuccess
  }
}
