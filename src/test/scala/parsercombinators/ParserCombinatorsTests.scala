package parsercombinators

import org.scalatest.FunSuite
import parsercombinators.ParserCombinators._

class ParserCombinatorsTests extends FunSuite {

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

  test("many1") {
    val res = many1(digit)("12345!")
    assert(res.head._1.mkString == "12345")
    assert(res.head._2 == "!")

    val resEmpty = many1(digit)("!12345")
    assert(resEmpty.isEmpty)
  }

  test("many") {
    val res = many(digit)("12345!")
    assert(res.head._1.mkString == "12345")
    assert(res.head._2 == "!")

    val resEmpty = many(digit)("!12345")
    assert(resEmpty.head._1.isEmpty)
  }
}
