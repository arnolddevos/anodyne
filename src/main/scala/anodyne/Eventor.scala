package anodyne

trait Eventor extends HMaps with HLogs with Rules {

  type Corpus = CellList

  def addResult(h: CellList, t: Term)(v: t.Value) = h.add(cell(t)(v))
  def removeResult(h: CellList, t: Term) = h.removeFirst(t)

  abstract class Term extends TermSpec { term =>
    def name: String
    def unapply(h: CellList): Option[Value] = h.get(term)
    object Events {
      def unapplySeq(h: CellList): Option[Seq[Cell]] = Some(h.findAll(term))
    }
    override def toString = s"Term($name)"
  }

  def term[A](s: Symbol): Term { type Value = A } = new Term {
    type Value = A
    val name = s.name
  }

}
