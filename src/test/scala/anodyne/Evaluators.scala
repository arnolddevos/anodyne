package anodyne

import Matching._

trait Evaluators { this: Rules with HMaps =>
  
  trait Evaluator { evaluator =>
    type Data = Row
    type Value 
    val depends: List[Term]
    val result: Term { type Value = evaluator.Value }
    val body: Data => Option[result.Value]

    trait Input[V] extends Extractor[Data, V] {
      val term: Term { type Value = V }
      def unapply(d: Data): Option[V] = d.get(term)
    }

    def extend(t: Term): (Evaluator, Input[t.Value]) = {
      def i = new Input[t.Value] {
        val term = t: Term { type Value = t.Value }
      }
      def e = new Evaluator {
        type Value = evaluator.Value
        val depends = t :: evaluator.depends
        val result = evaluator.result
        val body = evaluator.body
      }
      (e, i)
    }

    def define( f: Data => Option[Value]) = new Evaluator {
      type Value = evaluator.Value
      val depends = evaluator.depends
      val result = evaluator.result
      val body = f
    }
  }

  object Evaluator {
    def apply(t: Term): Evaluator { type Value = t.Value } = new Evaluator {
      type Value = t.Value
      val depends = Nil
      val result = t: Term { type Value = t.Value }
      val body = (_: Data) => None
    }
  }
}
