package calculator

import org.scalatest.FunSuite

class CalculatorTests extends FunSuite {
  test("simple") {
    val calc = new Calculator
    val res = calc.evaluate("1+1")
    assert(res.get == 2)
  }

  test("parens") {
    val calc = new Calculator
    val res = calc.evaluate("1+(3-2)")
    assert(res.get == 2)
  }
}
