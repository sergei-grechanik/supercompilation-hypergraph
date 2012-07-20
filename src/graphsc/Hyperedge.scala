package graphsc

sealed trait Label

case class Construct(name: String)
  extends Label

case class Scrutinize(cases: List[(String, Int)])
  extends Label

case class Apply
  extends Label
  
case class Id
  extends Label


case class Renaming(vector: Vector[Int]) {
  def comp(other: Renaming): Renaming =
    Renaming(other.vector.map(vector(_)))
}



case class Edge(dest: Node, renaming: Renaming, tick: Boolean = false)
case class Hyperedge(source: Node, dests: List[Edge])

class Node {
  val outs = collection.mutable.Set[Hyperedge]()
  val ins = collection.mutable.Set[Hyperedge]()
}

class Hypergraph {
  val nodes = collection.mutable.Set[Node]()
}
