package anodyne

object RuleExamples extends App with Rules with HMaps {

  abstract class Term extends TermSpec {
    def name: String
    def unapply(h: HMap): Option[Value] = h.get(this)
    override def toString = s"Term($name)"
  }

  def term[A](s: Symbol): Term { type Value = A } = new Term {
    type Value = A
    val name = s.name
  }

  object & {
    def unapply[X](x: X) = Some((x, x))
  }

  val Attention = term[String]('Attention)
  val Title = term[String]('Title)
  val Patient = term[String]('Patient)
  val Method = term[String]('Method)
  val BatchCount = term[Int]('BatchCount)
  val Amount = term[Double]('Amount)
  val GST = term[Double]('GST)
  val FeeExGST = term[Double]('FeeExGST)
  val OpeningBalance = term[Double]('OpeningBalance)
  val Balance = term[Double]('Balance)
  val Paid = term[Double]('Paid)

  val rules = List(
    rulePf(Attention)  { case Title(a) & Patient(p) => s"$a $p" },
    rulePf(Attention)  { case Patient(p) => p },
    rulePf(Title)      { case _ => "Miss" },
    rulePf(BatchCount) { case Method("nogap") => 1 },
    rulePf(GST)        { case FeeExGST(f) => f * 0.1 },
    rulePf(GST)        { case Amount(a) => a / 11.0 },
    rulePf(Amount)     { case FeeExGST(f) & GST(g) => f+g },
    rulePf(FeeExGST)   { case Amount(a) & GST(g) => a-g },
    rulePf(OpeningBalance) { case Amount(a) => a },
    rulePf(Balance)    { case OpeningBalance(a) & Paid(p) => a-p }
  )

  def show(h: HMap) = {
    for(t <- h.keys.toSeq.sortBy(_.name); v = h(t))
      println(s"$t\t= $v")
  }

  val tests = List[HMap => HMap] (
    _.add(Patient)("Jim Jones"),
    _.add(Title)("Mr"),
    _.add(Amount)(110.00),
    HMap(_, Patient, Title),
    _.add(FeeExGST)(20.00),
    _.add(Paid)(10.00),
    _ => HMap().add(Method)("nogap")
  )

  var h = HMap()
  for(test <- tests) {
    h = test(h)
    println("---")
    show(applyRules(h, rules))
  }

}
