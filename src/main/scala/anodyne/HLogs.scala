package anodyne

trait HLogs extends Cells { this: HMaps =>

  trait Cell extends CellSpec {
    val stamp: Long
  }

  def cell(t: Term, s: Long = System.currentTimeMillis)(v: t.Value) = new Cell {
    type Value = t.Value
    val term = t: Term { type Value = t.Value }
    val value = v
    val stamp = s
  }
}

trait HMultiMaps extends Cells { this: HMaps =>

  trait Cell extends CellSpec

  def cell(t: Term)(v: t.Value): Cell = new Cell {
    type Value = t.Value
    val term = t: Term { type Value = t.Value }
    val value = v
  }
}

trait Cells { this: HMaps =>

  type Cell <: CellSpec

  trait CellSpec { cell =>
    type Value
    def value: Value
    val term: Term { type Value = cell.Value }
    def cast(t: Term) =
      if(t == term) Some(asInstanceOf[Cell { type Value = t.Value }])
      else None
    override def toString = s"$term = $value"
  }

  trait CellList extends Row { self =>
    def underlying: List[Cell]

    def find(t: Term) =
      underlying.collectFirst(Function.unlift(_.cast(t)))
    def findAll(t: Term) =
      underlying.collect(Function.unlift(_.cast(t)))
    def keys =
      underlying.map( _.term ).distinct
    def get(t: Term): Option[t.Value] =
      find(t).map( _.value )
    def apply(t: Term): t.Value =
      get(t).get
    def add(c: Cell) = new CellList {
      val underlying = c :: self.underlying
    }
    def remove(t: Term): CellList = new CellList {
      val underlying = self.underlying filter (_.term == t)
    }
    def removeFirst(t: Term): CellList = new CellList {
      val underlying = {
        val (a, b) = self.underlying span (_.term != t)
        a ::: b.drop(1)
      }
    }
    def filter(p: Cell => Boolean) = new CellList {
      val underlying = self.underlying filter p
    }
  }

  object CellList {
    def apply() = empty
    val empty: CellList = new CellList { def underlying = Nil }
  }
}
