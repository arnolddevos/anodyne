package anodyne

trait HMaps {

  type Term <: TermSpec

  abstract class TermSpec {
    type Value
    final override def equals(x: Any) = super.equals(x)
    final override def hashCode(): Int = super.hashCode()
  }

  trait HMap { self =>
    def underlying: Map[Term, Any]
    def get(t: Term): Option[t.Value] = underlying.get(t).asInstanceOf[Option[t.Value]]
    def apply(t: Term): t.Value = underlying(t).asInstanceOf[t.Value]
    def add(t: Term)( v: t.Value): HMap = new HMap {
      val underlying = self.underlying.updated(t, v)
    }
    def &(h: HMap): HMap = new HMap {
      val underlying = self.underlying ++ h.underlying
    }
    def remove(t: Term): HMap = new HMap {
      val underlying = self.underlying - t
    }
    def keys: Iterable[Term] = underlying.keys
    def isEmpty = underlying.isEmpty
  }

  object HMap {
    def apply() = empty
    val empty: HMap = new HMap { def underlying = Map.empty }
    def apply( row: HMap, ts: Term* ): HMap =
      ts.foldLeft(HMap())((m, t) => m.add(t)(row(t)))
  }
}
