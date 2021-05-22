package parsercombinators

import org.scalatest.FunSuite
import parsercombinators.ParserCombinators._

class ParserCombinatorsTests extends FunSuite {

  def digit: Parser[Char] = sat(('0' to '9').contains)
  def upper: Parser[Char] = sat(('A' to 'Z').contains)
  def lower: Parser[Char] = sat(('a' to 'z').contains)
  def letter: Parser[Char] = lower <|> upper
  def alphanum: Parser[Char] = letter <|> digit

  def word: Parser[String] = {
    val neWord: Parser[String] = for {
      x <- letter
      xs <- word
      w = x + xs
    } yield w
    neWord <|> result("")
  }

  test("digit") {
    val succeedsOK = digit("666")
    assert(succeedsOK.nonEmpty)
    assert(succeedsOK.head._1 == '6')

    val fails = digit("Hello there")
    assert(fails.isEmpty)
  }

  test("uppercase") {
    val res = upper("Hello there")
    assert(res.nonEmpty)
    assert(res.head._1 == 'H')

    val fails = upper("general kenobi")
    assert(fails.isEmpty)
  }

  test("lowercase") {
    val res = lower("general kenobi")
    assert(res.nonEmpty)
    assert(res.head._1 == 'g')

    val fails = lower("Hello there")
    assert(fails.isEmpty)
  }

  test("sequence") {
    val parseExcelCellId = upper ~> digit
    val res = parseExcelCellId("A3")
    assert(res.nonEmpty)
    assert(res.head._1 == ('A', '3'))

    val onlyNumbersRes = parseExcelCellId("33")
    assert(onlyNumbersRes.isEmpty)

    val onlyLettersRes = parseExcelCellId("AA")
    assert(onlyLettersRes.isEmpty)

    val tooShortRes = parseExcelCellId("A")
    assert(tooShortRes.isEmpty)
  }

  test("alphanum") {
    val numbers = alphanum("123")
    assert(numbers.nonEmpty)
    assert(numbers.head._1 == '1')

    val lowercase = alphanum("abc")
    assert(lowercase.nonEmpty)
    assert(lowercase.head._1 == 'a')

    val uppercase = alphanum("ABC")
    assert(uppercase.nonEmpty)
    assert(uppercase.head._1 == 'A')

    val somethingElse = alphanum("###")
    assert(somethingElse.isEmpty)
  }

  test("word") {
    val res = word("Yes!")
    assert(res.length == 4)
    assert(res.head._1 == "Yes")
    assert(res.head._2 == "!")
  }
}
