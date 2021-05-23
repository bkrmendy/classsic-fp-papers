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
    case _  => char(s.head) >> string(s.tail) >> result(s)
  }
}

