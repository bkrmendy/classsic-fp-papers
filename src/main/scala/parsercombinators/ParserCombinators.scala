package parsercombinators

object ParserCombinators {
  type Parser[T] = (String) => Seq[(T, String)]

  def result[T](a: T): Parser[T] = (input: String) => List((a, input))

  def zero[T]: Parser[T] = (_) => List.empty

  def item: Parser[Char] = (input: String) => input match {
    case "" => List()
    case _ => List((input.head, input.tail))
  }

  def seq[T, U](a: Parser[T], b: Parser[U]): Parser[(T, U)] = (input: String) => {
    for {
      (v, inp1) <- a(input)
      (w, inp2) <- b(inp1)
    } yield ((v,w), inp2)
  }

  def bind[T, U](a: Parser[T], f: (T) => Parser[U]): Parser[U] = (input: String) => {
    val ps = for {
      (v, inp1) <- a(input)
    } yield f(v)(inp1)
    ps.flatten
  }

  def sat(pred: Char => Boolean): Parser[Char] = {
    val cont = (x: Char) => if (pred(x)) {
      result(x)
    } else {
      zero
    }
    bind(item, cont)
  }

  def plus[T](a: Parser[T], b: Parser[T]): Parser[T] = (input: String) => a(input).concat(b(input))


}

