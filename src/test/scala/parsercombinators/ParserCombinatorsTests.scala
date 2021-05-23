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

  test("nat") {
    val res = nat("123")
    assert(res.nonEmpty)
    assert(res.head._1 == 123)
    assert(res(1)._1 == 12)

    val resFails = nat("abba")
    assert(resFails.isEmpty)
  }

  test("string") {
    val parser = string("baba")
    val res = parser("baba")
    assert(res.nonEmpty)
    assert(res.head._1 == "baba")

    val resFail = parser("baby")
    assert(resFail.isEmpty)
  }

  test("int") {
    val meta = (input: String, expected: Int) => {
      val res = int(input)
      assert(res.head._1 == expected)
    }

    meta("+123", 123)
    meta("-123", -123)
    meta("123", 123)
  }

  test("char") {
    val parser = char('1')
    val res = parser("111")
    assert(res.nonEmpty)
    assert(res.head._1 == '1')

    val resFail = parser("aa")
    assert(resFail.isEmpty)
  }

  test("bracket") {
    val parser = bracket(char('<'), int, char('>'))
    val res = parser("<123>")
    assert(res.nonEmpty)
    assert(res.head._1 == 123)

    val resFail = parser("231")
    assert(resFail.isEmpty)

    val resEmpty = parser("")
    assert(resEmpty.isEmpty)
  }

  test("sepBy") {
    val parser = sepBy(int, char(','))
    val res = parser("1,2,3,4,5")
    assert(res.nonEmpty)
    assert(res.head._1 == List(1,2,3,4,5))
  }

  test("list of ints") {
    val ints = sepBy(int, char(','))
    val parser = bracket(char('['), ints, char(']'))
    val res = parser("[1,2,3,4,5]")
    assert(res.nonEmpty)
    assert(res.head._1 == List(1,2,3,4,5))
  }
}
