package graphsc

trait Canonizer extends Hypergraph {
  
  private def addHyperedgeSuper(h: Hyperedge): Node =
      super.addHyperedge(h)
  
  private def newNodeSuper(a: Int): Node =
      super.newNode(a)
      
  private object superthis extends Transformations {
    def addHyperedge(h: Hyperedge): Node =
      addHyperedgeSuper(h)
      
    def newNode(a: Int): Node =
      newNodeSuper(a)
  }
  
  abstract override def newNode(arity: Int): Node =
    super.newNode(arity)
  
  abstract override def addHyperedge(h: Hyperedge): Node = {
    if(h.label.isInstanceOf[Var])
      superthis.addHyperedge(h)
    superthis.throughRenamings(h)
    h.source.realNode
  }
}

trait RenamingComposer extends Hypergraph {
  
  private def addHyperedgeSuper(h: Hyperedge): Node =
      super.addHyperedge(h)
  
  private def newNodeSuper(a: Int): Node =
      super.newNode(a)
      
  private object superthis extends Transformations {
    def addHyperedge(h: Hyperedge): Node =
      addHyperedgeSuper(h)
      
    def newNode(a: Int): Node =
      newNodeSuper(a)
  }
  
  abstract override def newNode(arity: Int): Node
  abstract override def addHyperedge(h: Hyperedge): Node
  
  override def onNewHyperedge(h: Hyperedge) {
    super.onNewHyperedge(h)
    if(h.label.isInstanceOf[Renaming])
      for(h2 <- h.dests(0).outs if h2.label.isInstanceOf[Renaming])
        superthis.renamingRenaming(h, h2)
  }
}