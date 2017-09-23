package anondyne
package xml


sealed trait Node {
  def child: List[Node]
  def serialize: List[String]
  def inner = for(c <- child; l <- c.serialize) yield l
  def ~(rhs: Node): Node = Frag(this :: rhs :: Nil)
}

case class Frag(child: List[Node]) extends Node {
  def serialize = inner
}

case class Text( text: String) extends Node {
  def child = Nil
  def serialize = List(text)
}

case class Elem(tag: String, atts: List[(String, String)]=Nil, child: List[Node]=Nil) extends Node {
  def attsString = (for((n, v) <- atts) yield s"$n='$v'").mkString(" ")
  def open = s"<$tag $attsString>"
  def close = s"</$tag>"
  def serialize =  open :: inner ::: (close :: Nil)
  def apply(rhs: Node) = copy(child=child ::: (rhs :: Nil))
  def apply(rhs: (String, String)*) = copy(atts=atts ::: rhs.toList)
}
