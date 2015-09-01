package anodyne
import scala.util.control.NonFatal

object Catching {
  
  def logExceptions[T](log: Throwable => Unit)( f: => T): T = 
    try { f } catch { case NonFatal(e) => log(e); throw e }

  def boxExceptions[T]( f: => T): Either[Throwable,T] =
    try { Right(f) } catch { case NonFatal(e) => Left(e) }
    
  def unboxExceptions[T]( v: Either[Throwable, T]) = v match {
    case Right(t) => t
    case Left(t) => throw t
  }
    
  def skipExceptions[T]( f: => T) = 
    try { Some(f) } catch { case NonFatal(e) => None } 
  
  def using[R <: java.io.Closeable, S](r: R)(f: R => S): S = {
    try f(r) finally r.close()
  }
}
