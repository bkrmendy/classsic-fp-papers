package parsercombinators

import org.scalatest.FunSuite
import parsercombinators.ParserCombinators._

class ParserCombinatorsTests extends FunSuite {

  def digit: Parser[Char] = sat((c: Char) => ('0' to '9').contains(c))
  def upper: Parser[Char] = sat((c: Char) => ('A' to 'Z').contains(c))
  def lower: Parser[Char] = sat((c: Char) => ('a' to 'z').contains(c))

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
}
