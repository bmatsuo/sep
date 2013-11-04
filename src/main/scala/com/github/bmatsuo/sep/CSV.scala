package com.github.bmatsuo.sep

import org.parboiled.scala._
import org.parboiled.scala.parserunners.ReportingParseRunner
import scalaz.{NonEmptyList, Validation}
import scalaz.std.anyVal._
import scalaz.syntax.id._
import scalaz.syntax.semigroup._
import scalaz.syntax.validation._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.std.list._
import sun.io.CharToByteUnicode

trait CSV {
  import CSV._

  val comma: Char
  val quote: Char
  val qesc: String

  private lazy val grammar = new Parser {
    def CSV: Rule1[Table] = rule (
      Table ~ EOI
    )

    def Table: Rule1[Table] = rule (
      oneOrMore(Row, separator = NewLine)
      ~~> (cs ⇒ NonEmptyList(cs.head, cs.tail:_*))
    )

    def Row: Rule1[Row] = rule (
      oneOrMore(Cell, separator = Comma)
      ~~> (cs ⇒ NonEmptyList(cs.head, cs.tail:_*))
    )

    def Cell: Rule1[String] = rule (
      QuotedCell
      | UnquotedCell
      | (str("") ~> identity)
    )

    def UnquotedCell: Rule1[String] = rule (
      oneOrMore(!NewLine ~ !Comma ~ NonQuote)
      ~~> (_.mkString)
    )

    def QuotedCell: Rule1[String] = rule (
      QuotedString
      | EmptyQuotes
    )

    def QuotedString: Rule1[String] = rule (
      Quote
      ~ oneOrMore(EscapedQuote | NonQuote)
      ~ Quote
      ~~> (_.mkString)
    )

    def EmptyQuotes: Rule1[String] = rule (
      Quote ~ Quote
      ~> identity
    )

    def NonQuote: Rule1[String] = rule (
      !Quote
      ~ ANY
      ~> identity
    )

    def Comma: Rule0 = rule (
      str(comma.toString)
    )

    def Quote: Rule0 = rule (
      str(quote.toString)
    )

    def EscapedQuote: Rule1[String] = rule (
      str(qesc.toString)
      ~> (_ ⇒ "\"")
    )

    def NewLine: Rule0 = rule (
      str("\r\n")
      | str("\r") // is "\r" reasonable to accept?
      | str("\n")
    )
  }

  def load(s: String): Validation[NonEmptyList[CSVParseError], Table] = {
    val result = ReportingParseRunner(grammar.CSV).run(s)
    val status = result.result.map { _.successNel[CSVParseError] }
    def errors =
      for {
        err  ← result.parseErrors
        pos  = err.getInputBuffer.getPosition(err.getEndIndex) // need something fancier to always get the right index?
        perr = CSVParseError(pos.line, pos.column, Option(err.getErrorMessage))
      } yield perr.wrapNel.fail[Table]

    status | errors.reduce(_ |+| _)
  }

  import scalaz._
  import Scalaz._

  def export(csv: Table): String =
    csv.map(exportRow).stream.mkString("\r\n")

  def exportRow(r: Row): String  =
    r.map(quoteEscape).stream.mkString(comma.toString)

  def quoteEscape(s: String): String = {
    val escaped = s.flatMap(c ⇒ (c == quote) ? qesc | c.toString)

    (s == escaped) ? s | quote.toString + escaped + quote.toString
  }
}

object CSV {
  type Table = NonEmptyList[Row]
  type Row = NonEmptyList[String]

  // an RFC4180 compliant CSV implementation
  val rfc4180 = new CSV {
    val comma = ','
    val quote = '"'
    val qesc = "\"\""
  }

  val empty = NonEmptyList(NonEmptyList(""))
}
