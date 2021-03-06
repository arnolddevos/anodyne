package anodyne

import scala.util.matching.Regex
import java.time._
import Catching._

trait Matching {
  
  trait Extractor[-S,+T] { X =>
    def unapply(s: S): Option[T]
    
    def seq = seqExtractor {
      (ss: Seq[S]) =>
        val ts = for( X(t) <- ss) yield t
        if( ts.isEmpty) None else Some(ts)
    }
    
    def withDefault[D >: T](default: D) = extractor { 
      (s: S) => unapply(s) orElse Some(default)
    }
    
    /**
     * Always matches but yields and Option.
     */
    lazy val option = extractor {
      (s: S) => Some(unapply(s))
    }
    
    lazy val unmatched = predicate {
      (s: S) => unapply(s).isEmpty
    }
    
    def *[U]( e: Extractor[T, U]) = extractor {
      (s: S) => for( t <- unapply(s); u <- e.unapply(t)) yield u 
    }
    
    def *[U]( e: SeqExtractor[T, U]) = seqExtractor {
      (s: S) => for( t <- unapply(s); us <- e.unapplySeq(t)) yield us 
    }
  }
  
  trait Converter[T, S] extends (T => S) with Extractor[S, T]
  
  trait SeqExtractor[-S,+T] { X =>
    def unapplySeq(s: S): Option[Seq[T]]
    
    def seq = seqExtractor {
      (ss: Seq[S]) =>
        val ts = for( X(ts @_*) <- ss; t <- ts) yield t
        if( ts.isEmpty) None else Some(ts)
    }

    def withDefault[D >: T](default: Seq[D]) = seqExtractor { 
      (s: S) => unapplySeq(s) orElse Some(default)
    }
    
    def *[U]( e: Extractor[T, U]) = seqExtractor {
      (s: S) => for( ts <- unapplySeq(s)) yield for( t <- ts; u <- e.unapply(t).toList) yield u 
    }
    
    def *[U]( e: SeqExtractor[T, U]) = seqExtractor {
      (s: S) => for( ts <- unapplySeq(s)) yield for( t <- ts; us <- e.unapplySeq(t).toList; u <- us) yield u  
    }
    
    lazy val option = extractor {
      (s: S) => Some(unapplySeq(s))
    }
    
    lazy val unmatched = predicate {
      (s: S) => unapplySeq(s).isEmpty
    }
    
    lazy val first = extractor {
      (s: S) => unapplySeq(s) flatMap (_.headOption)
    }
  }

  implicit class RegexSeqExtractor( re: Regex ) extends SeqExtractor[String, String] {
    def unapplySeq( s: String ) = re.unapplySeq(s)
  }
  
  trait Predicate[-S] {
    def unapply(s: S): Boolean
  }
  
  def extractor[S,T]( f: S => Option[T] ): Extractor[S, T] = 
    new Extractor[S, T] { def unapply(s: S) = f(s) }
  
  def seqExtractor[S,T]( f: S => Option[Seq[T]] ): SeqExtractor[S, T] = 
    new SeqExtractor[S, T] { def unapplySeq(s: S) = f(s) }
  
  def predicate[S]( p: S => Boolean ): Predicate[S] = 
    new Predicate[S] { def unapply(s: S) = p(s) }

  def seqPattern[S,T]( pf: PartialFunction[S, Seq[T]] ) = seqExtractor[S, T]( pf.lift )

  def pattern[S,T]( pf: PartialFunction[S, T]) = extractor[S, T]( pf.lift )
  
  val LongValue = extractor { v: String => skipExceptions { v.toLong }}
  
  val IntValue = extractor { v: String => skipExceptions {  v.toInt }}

  val DoubleValue = extractor { v: String => skipExceptions {  v.toDouble }}
  
  val InstantValue = extractor{ v: String => skipExceptions { Instant.parse(v) }}

  val LocalDateTimeValue = extractor{ v: String => skipExceptions { LocalDateTime.parse(v) }}

  val LocalDateValue = extractor{ v: String => skipExceptions { LocalDate.parse(v) }}

  val LocalTimeValue = extractor{ v: String => skipExceptions { LocalTime.parse(v) }}

  val ZoneOffsetValue = extractor{ v: String => skipExceptions { ZoneOffset.of(v) }}

  val DurationValue = extractor{ v: String => skipExceptions { Duration.parse(v) }}

  def Split( sep: String) = seqExtractor[String, String] {
    s => Some(s.split(sep))
  }
  
  object & {
    def unapply[A](a: A): Option[(A, A)] = Some(a, a)
  }
}

object Matching extends Matching
