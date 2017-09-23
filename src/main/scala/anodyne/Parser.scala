package anondyne

class Parser[I] {
  type Input = I

  import State._

  sealed trait State[+A] {
    def flatMap[B](f: A => State[B]): State[B] = Sequence(this, f)
  }

  object State {
    case class Sequence[A, B](sa: State[A], f: A => State[B]) extends State[B]
    case class Read[A](f: Input => State[A], eoi: State[A]) extends State[A]
    case class Success[A](value: () => A) extends State[A]
    case class Failue(message: String) extends State[Nothing]
  }

  def parse[A0](x: Iterator[Input])(p: State[A0]): Either[String, A0] = {

    def reenter[A](s: State[A]): Either[String, A] = loop(s)

    @annotation.tailrec
    def loop[A](s0: State[A]): Either[String, A] = {
      s0 match {
        case Sequence(s, f) => reenter(s).flatMap( a => reenter(f(a)))
        case Read(f, s) => loop(if(x.hasNext) f(x.next) else s)
        case Success(a) => Right(a())
        case Failue(m) => Left(m)
      }
    }
    loop(p)
  }
  
  def read[A](f: Input => State[A]): State[A] = 
    Read(f, Failue("unexpected end of input"))
  def readOrElse[A](f: Input => State[A])( a: => A): State[A] = 
    Read(f, Success(() => a))
  def endOfInput[A](a: => A): State[A] = 
    Read(_ => Failue("expected end of input"), Success(() => a))

}
