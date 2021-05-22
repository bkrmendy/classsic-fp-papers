package parsercombinators

object ParserCombinators {
  def result[T](a: T): Parser[T] = (input: String) => List((a, input))

  def zero[T]: Parser[T] = _ => List.empty

  def item: Parser[Char] = {
    case "" => List()
    case input => List((input.head, input.tail))
  }

  def sat(pred: Char => Boolean): Parser[Char] = {
    val cont: Char => Parser[Char] = (x: Char) => if (pred(x)) result(x) else zero
    item >>= cont
  }
}

