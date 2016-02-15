package anodyne

trait HMaps {

  type Term <: { type Value }

  trait Row {
    def get(t: Term): Option[t.Value]
    def apply(t: Term): t.Value
  }

  trait HMap extends Row { self =>
    protected def underlying: Map[Term, Any]
    def get(t: Term): Option[t.Value] = underlying.get(t).asInstanceOf[Option[t.Value]] 
    def apply(t: Term): t.Value = underlying(t).asInstanceOf[t.Value]
    def add(t: Term)( a: t.Value): HMap = new HMap { 
      val underlying = self.underlying.updated(t, a) 
    }
    def remove(t: Term): HMap = new HMap { 
      val underlying = self.underlying - t
    }
    def keys: Iterable[Term] = underlying.keys
  }

  object HMap {
    def apply(): HMap = new HMap {
      protected def underlying = Map.empty
    }

    def apply( row: Row, ts: Term* ): HMap = 
      ts.foldLeft(HMap())((m, t) => m.add(t)(row(t)))
  }
}
