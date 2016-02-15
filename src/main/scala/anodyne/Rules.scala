package anodyne

trait Rules { this: HMaps =>

  trait Rule { rule =>
    type Value
    val head: Term { type Value = rule.Value }
    val body: HMap => Option[Value]
    def orElse (alt: HMap => Option[Value]): Rule = ruleOf(head) {
      m => body(m) orElse alt(m)
    }
    override def toString = s"rule($head)"
  }

  def rulePf(h: Term)( b: PartialFunction[HMap, h.Value]): Rule = 
    ruleOf(h)(b.lift)

  def ruleOf(h: Term)( b: HMap => Option[h.Value]): Rule = {
    new Rule {
      type Value = h.Value
      val head = h: Term { type Value = h.Value }
      val body = b
    }
  }

  def applyRules(h: HMap, rs: List[Rule]): HMap = {

    val rs0: Iterable[Rule] = 
      for {
        (k, rsk) <- rs.groupBy(_.head) 
        if h.get(k).isEmpty
      }
      yield 
        rsk.reduce((r1, r2) => r1 orElse r2.body.asInstanceOf[HMap => Option[r1.Value]])

    @annotation.tailrec
    def iterate(h0: HMap): HMap = {
      val (h, fired) =
        rs0.foldLeft((h0, false)) {
          (hf, r) =>
            val (h, fired) = hf
            val k = r.head
            val e = r.body(h)

            if( e != h.get(k))
              e match {
                case Some(a) => (h.add(k)(a), true)
                case None => (h.remove(k), true)
              }
            else
              (h, fired)
        }

      if(fired) iterate(h) else h
    }

    iterate(h)
  }
}
