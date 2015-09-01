package anodyne

import scala.language.higherKinds

trait NaturalMap[K[_], V[_]] { parent =>

  protected val underlying: Map[K[_], V[_]]

  def get[T]( key: K[T]) = 
    underlying.get(key).asInstanceOf[Option[V[T]]]
  
  def updated[T]( key: K[T], value: V[T]) = new NaturalMap[K, V] {
    protected val underlying = parent.underlying.updated(key, value)
  }
}

object NaturalMap { 
  def empty[K[_], V[_]] = new NaturalMap[K, V] { 
    protected val underlying = Map.empty[K[_], V[_]]
  }
  def unsafe[K[_], V[_]]( base: Map[K[_], V[_]]) = new NaturalMap[K, V] { 
    protected val underlying = base
  }
}
