package anodyne

trait Rules { this: HMaps =>

  type Corpus <: Row

  def addResult(h: Corpus, t: Term)(v: t.Value): Corpus
  def removeResult(h: Corpus, t: Term): Corpus

  trait Rule { rule =>
    type Value
    val term: Term { type Value = rule.Value }
    val body: Corpus => Option[Value]
    def orElse (alt: Corpus => Option[Value]): Rule = ruleOf(term) {
      m => body(m) orElse alt(m)
    }
    override def toString = s"rule($term)"
  }

  def rulePf(h: Term)( b: PartialFunction[Corpus, h.Value]): Rule = 
    ruleOf(h)(b.lift)

  def ruleOf(h: Term)( b: Corpus => Option[h.Value]): Rule = {
    new Rule {
      type Value = h.Value
      val term = h: Term { type Value = h.Value }
      val body = b
    }
  }

  def applyRules(h: Corpus, rs: List[Rule]): Corpus = {

    val rs0: Iterable[Rule] = 
      for {
        (k, rsk) <- rs.groupBy(_.term) 
        if h.get(k).isEmpty
      }
      yield 
        rsk.reduce(
          (r1, r2) => 
            r1 orElse r2.body.asInstanceOf[Corpus => Option[r1.Value]])

    @annotation.tailrec
    def iterate(h0: Corpus): Corpus = {
      val h =
        rs0.foldLeft(h0) {
          (h, r) =>
            val k = r.term
            val e = r.body(h)
            if( e != h.get(k))
              e.fold(
                removeResult(h, k))(
                addResult(h, k)(_))
            else
              h
        }

      if(h != h0) iterate(h) else h
    }

    iterate(h)
  }
}
