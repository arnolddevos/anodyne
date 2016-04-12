package anodyne

trait Rules { this: HMaps =>

  trait Rule { rule =>
    type Value
    val term: Term { type Value = rule.Value }
    val body: HMap => Option[Value]
    def orElse (alt: HMap => Option[Value]): Rule = ruleOf(term) {
      m => body(m) orElse alt(m)
    }
    override def toString = s"rule($term)"
  }

  def rulePf(h: Term)( b: PartialFunction[HMap, h.Value]): Rule =
    ruleOf(h)(b.lift)

  def ruleOf(h: Term)( b: HMap => Option[h.Value]): Rule = {
    new Rule {
      type Value = h.Value
      val term = h: Term { type Value = h.Value }
      val body = b
    }
  }

  def applyRules(h: HMap, rs: List[Rule]): HMap = {

    val rs0: Iterable[Rule] =
      for {
        (k, rsk) <- rs.groupBy(_.term)
        if h.get(k).isEmpty
      }
      yield
        rsk.reduce(
          (r1, r2) =>
            r1 orElse r2.body.asInstanceOf[HMap => Option[r1.Value]])

    @annotation.tailrec
    def iterate(h0: HMap): HMap = {
      val h =
        rs0.foldLeft(h0) {
          (h, r) =>
            val k = r.term
            val e = r.body(h)
            if( e != h.get(k))
              e.fold(h.remove(k))(h.add(k)(_))
            else
              h
        }

      if(h != h0) iterate(h) else h
    }

    iterate(h)
  }
}

object Rules extends Rules with HMaps {
  abstract class Term extends TermSpec
  def term[V] = new Term { type Value = V }
}
