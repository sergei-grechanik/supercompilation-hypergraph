package graphsc

class Node(initial_used: Set[Int]) {
  var mused = initial_used
  val mouts = collection.mutable.Set[Hyperedge]()
  val mins = collection.mutable.Set[Hyperedge]()
  var gluedTo: RenamedNode = null
  // Indicated that this node is in the process of gluing
  var beingGlued: Boolean = false
  
  var prettyDebug = ""
  
  private def error() =
    throw new RuntimeException("Using undereferenced glued node is not a good idea")
    
  def used: Set[Int] =
    if(gluedTo == null) mused else gluedTo.used
    
  def arity: Int =
    (used + (-1)).max + 1
    
  def isReal: Boolean =
    gluedTo == null
    
  def outs: Set[Hyperedge] = 
    if(gluedTo == null) mouts.toSet else error()
  def ins: Set[Hyperedge] = 
    if(gluedTo == null) mins.toSet else error()
  
  def outsMut: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mouts else error()
  def insMut: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mins else error()
    
  def ~~(n: Node): Boolean =
    if(gluedTo == null && n.gluedTo == null)
      this == n
    else if(gluedTo != null)
      gluedTo.node ~~ n
    else
      n ~~ this
    
  // Sometimes the node was glued to some other node...
  // Imperative programming sucks, I know
  def deref: RenamedNode =
    if(gluedTo == null) RenamedNode.fromNode(this)
    else gluedTo.deref
  
  def uniqueName: String =
    if(gluedTo == null)
      super.toString + "/" + arity
    else
      super.toString + "/" + arity + "(" + deref + ")"
    
  override def toString: String =
    uniqueName
}

class InvalidFreeNodeUseException extends 
  Exception("FreeNode should be glued and then dereferenced to be used safely")

// An auxiliary node that cannot belong to a hypergraph
class FreeNode(initial_used: Set[Int]) extends Node(initial_used) {
  override def uniqueName: String =
    "{" + super.uniqueName + "}"
    
  override def outs = throw new InvalidFreeNodeUseException
  override def ins = throw new InvalidFreeNodeUseException
  override def outsMut = throw new InvalidFreeNodeUseException
  override def insMut = throw new InvalidFreeNodeUseException
}
