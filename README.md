# anodyne
A small collection of useful things with no dependencies.

> /ˈanədʌɪn/ adjective: not likely to cause offense or disagreement and somewhat dull.

## Matching

The Matching package defines a trait:

```scala
trait Extractor[-S,+T] {
  def unapply(s: S): Option[T]
  ...
}
```

This captures the scala pattern matching convention in a trait. It is the inverse of `Function1[-S, +T]` and a close relative of `PartialFunction[-S, +T]`.  
