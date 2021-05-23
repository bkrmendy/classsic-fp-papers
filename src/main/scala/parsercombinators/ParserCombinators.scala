package parsercombinators

object ParserCombinators {
  def result[T](a: T): Parser[T] = (input: String) => List((a, input))

  def zero[T]: Parser[T] = _ => List.empty

  def item: Parser[Char] = {
    case "" => List()
    case input => List((input.head, input.tail))
  }

  def sat(pred: Char => Boolean): Parser[Char] = for {
    x <- item
    if pred(x)
  } yield x

  def char(c: Char): Parser[Char] = sat((cc: Char) => c == cc)

  def string(s: String): Parser[String] = s match {
    case "" => result("")
    case _ => char(s.head) >> string(s.tail) >> result(s)
  }

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

  def many1[T](p: Parser[T]): Parser[List[T]] = for {
    x <- p
    xs <- many(p)
  } yield x +: xs

  def many[T](p: Parser[T]): Parser[List[T]] = many1(p) <|> result(List.empty)

  def nat: Parser[Int] = for (xs <- many1(digit)) yield (xs.mkString.toInt)
}


