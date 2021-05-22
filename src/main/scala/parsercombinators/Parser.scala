package parsercombinators


trait Parser[T] {
  def apply(input: String): Seq[(T, String)]

  def map[U](f: T => U): Parser[U] = this >>= (a => ParserCombinators.result(f(a)))
  def flatMap[U](f: T => Parser[U]): Parser[U] = this >>= f

  def ~>[U](b: Parser[U]): Parser[(T, U)] = (input: String) => {
    for {
      (v, inp1) <- this(input)
      (w, inp2) <- b(inp1)
    } yield ((v,w), inp2)
  }

  def >>=[U](f: T => Parser[U]): Parser[U] = (input: String) => {
    val ps = for {
      (v, inp1) <- this(input)
    } yield f(v)(inp1)
    ps.flatten
  }

  def <|>(b: Parser[T]): Parser[T] = (input: String) => this(input).concat(b(input))
}
