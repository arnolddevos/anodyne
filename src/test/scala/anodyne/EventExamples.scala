package anodyne

object EventExamples extends HMaps with HLogs {

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

  val e = cell(GST)(33.3)
  println(e)
}
