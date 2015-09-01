package anodyne

trait DisjointSets[T] {
  def sets: Map[T, Set[T]]
  def find(t: T): Option[(T, Set[T])] = sets find { case (_, ts) => ts contains t }
  def add(t: T): DisjointSets[T] = union(t, t)
  def union(t1: T, t2: T): DisjointSets[T] = {
    val sets1 = (find(t1), find(t2)) match {
      case (Some((r1, s1)), Some((r2, s2))) => sets + (r1 -> (s1 union s2)) - r2
      case (Some((r1, s1)), None) => sets + (r1 -> (s1 + t2))
      case (None, Some((r2, s2))) => sets + (r2 -> (s2 + t1))
      case (None, None) => sets + (t1 -> Set(t1, t2))
    }
    new DisjointSets[T] { val sets = sets1 }
  }
}

object DisjointSets {
  def empty[T] = new DisjointSets[T] { def sets = Map.empty[T, Set[T]] }
}
