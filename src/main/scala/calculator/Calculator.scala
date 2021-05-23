package calculator

import parsercombinators.Parser
import parsercombinators.ParserCombinators._

class Calculator {
  private def add = (a: Int, b: Int) => a + b
  private def sub = (a: Int, b: Int) => a - b

  private def addOp: Parser[(Int, Int) => Int] = for (_ <- char('+')) yield add
  private def subsOp: Parser[(Int, Int) => Int] = for (_ <- char('-')) yield sub

  private def openParen: Parser[Char] = char('(')
  private def closParen: Parser[Char] = char(')')

  private def expr: Parser[Int] = factor <== binaryOp
  private def binaryOp: Parser[(Int, Int) => Int] = addOp <|> subsOp
  private def factor: Parser[Int] = nat <|> bracket(openParen, expr, closParen)

  def evaluate(src: String): Option[Int] = expr(src) match {
    case Nil => None
    case (res, _)::_ => Some(res)
  }
}
