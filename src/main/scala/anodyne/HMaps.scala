package anodyne

trait HMaps { hmaps =>

  type Term <: TermSpec

  abstract class TermSpec { 
    type Value
    final override def equals(x: Any) = super.equals(x)
    final override def hashCode(): Int = super.hashCode()
  }

  trait Row {
    def get(t: Term): Option[t.Value]
    def apply(t: Term): t.Value
  }

  trait HMap extends Row { self =>
    def underlying: Map[Term, Any]
    def get(t: Term): Option[t.Value] = underlying.get(t).asInstanceOf[Option[t.Value]] 
    def apply(t: Term): t.Value = underlying(t).asInstanceOf[t.Value]
    def add(t: Term)( v: t.Value): HMap = new HMap { 
      val underlying = self.underlying.updated(t, v) 
    }
    def remove(t: Term): HMap = new HMap { 
      val underlying = self.underlying - t
    }
    def keys: Iterable[Term] = underlying.keys
  }

  object HMap {
    def apply() = empty
    val empty: HMap = new HMap { def underlying = Map.empty }
    def apply( row: Row, ts: Term* ): HMap = 
      ts.foldLeft(HMap())((m, t) => m.add(t)(row(t)))
  }
}
