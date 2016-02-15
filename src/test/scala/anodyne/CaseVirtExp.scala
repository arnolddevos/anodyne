package anodyne

object CaseVirtExp {

  sealed abstract class OptionIsh[+A] {
    def get: A
    def isEmpty: Boolean
    // def orElse[B >: A](alternative: ⇒ OptionIsh[B]): OptionIsh[B]
    // final def fold[B](ifEmpty: => B)(f: A => B): B = try if (isEmpty) ifEmpty else f(get) finally println("fold")
    // def flatMap[B](f: A => Option[B]): Option[B] = try if (isEmpty) None else f(get) finally println("flatMap")
  }

  case class SomeIsh[+A](private val a: A) extends OptionIsh[A] {
    def get = try a finally println("get = " + a)
    def isEmpty = try false finally println("Some.isEmpty")
    def orElse[B >: A](alternative: ⇒ OptionIsh[B]): OptionIsh[B] = try this finally println("Some.orElse")
  }

  case object NoneIsh extends OptionIsh[Nothing] {
    def get = ???
    def isEmpty = try true finally println("None.isEmpty")
    def orElse[B](alternative: ⇒ OptionIsh[B]): OptionIsh[B] = try alternative finally println("None.orElse")
  }

  object Extract {
    def unapply(i: Int): OptionIsh[String] = SomeIsh((i+1).toString)
  }

  object ExtractStr {
    def unapply(s: String): OptionIsh[String]  = SomeIsh(s +" ok")
  }

  object ExtractNo {
    def unapply(i: Int): OptionIsh[String] = NoneIsh
  }

  val f: PartialFunction[Int, String] = {
    case ExtractNo(_) | ExtractNo(_) => ""
    case Extract(ExtractStr(m))  => m
  }

  println(f(10))
}

// trait Intf {
//  type Rep[+T]
//  type M[+T] = Rep[Maybe[T]]

//  val __match: Matcher
//  abstract class Matcher {
//    // runs the matcher on the given input
//    def runOrElse[T, U](in: Rep[T])(matcher: Rep[T] => M[U]): Rep[U]

//    def zero: M[Nothing]
//    def one[T](x: Rep[T]): M[T]
//    def guard[T](cond: Rep[Boolean], then: => Rep[T]): M[T]
//    def isSuccess[T, U](x: Rep[T])(f: Rep[T] => M[U]): Rep[Boolean] // used for isDefinedAt
//  }

//  abstract class Maybe[+A] {
//    def flatMap[B](f: Rep[A] => M[B]): M[B]
//    def orElse[B >: A](alternative: => M[B]): M[B]
//  }

//  implicit def proxyMaybe[A](m: M[A]): Maybe[A]
//  implicit def repInt(x: Int): Rep[Int]
//  implicit def repBoolean(x: Boolean): Rep[Boolean]
//  implicit def repString(x: String): Rep[String]

//  def test = 7 match { case 5 => "foo" case _ => "bar" }
// }

// trait Impl extends Intf {
//  type Rep[+T] = String

//  object __match extends Matcher {
//    def runOrElse[T, U](in: Rep[T])(matcher: Rep[T] => M[U]): Rep[U] = ("runOrElse("+ in +", ?" + matcher("?") + ")")
//    def zero: M[Nothing]                                             = "zero"
//    def one[T](x: Rep[T]): M[T]                                      = "one("+x.toString+")"
//    def guard[T](cond: Rep[Boolean], then: => Rep[T]): M[T]          = "guard("+cond+","+then+")"
//    def isSuccess[T, U](x: Rep[T])(f: Rep[T] => M[U]): Rep[Boolean]  = ("isSuccess("+x+", ?" + f("?") + ")")
//  }

//  implicit def proxyMaybe[A](m: M[A]): Maybe[A] = new Maybe[A] {
//    def flatMap[B](f: Rep[A] => M[B]): M[B]                          = m + ".flatMap(? =>"+ f("?") +")"
//    def orElse[B >: A](alternative: => M[B]): M[B]                   = m + ".orElse("+ alternative +")"
//  }

//  def repInt(x: Int): Rep[Int] = x.toString
//  def repBoolean(x: Boolean): Rep[Boolean] = x.toString
//  def repString(x: String): Rep[String] = x
// }

// object Test extends Impl with Intf with App {
//   println(test)
// }
