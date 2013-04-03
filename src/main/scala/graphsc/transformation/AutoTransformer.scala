package graphsc.transformation

import graphsc._

trait AutoTransformer extends Hypergraph {
  var autoTransformations : List[PartialFunction[Hyperedge, Unit]] = List()
  override def onNewHyperedge(h: Hyperedge) {
    super.onNewHyperedge(h)
    for(t <- autoTransformations)
      t.lift(h)
  }
}

trait SelfLetAdder extends Hypergraph {
  var var0: RenamedNode = null
  var creating = false
  override def onNewNode(n: Node) {
    super.onNewNode(n)
    if(var0 == null && creating != true) {
      creating = true
      var0 = variable(0)
      add(Let(), var0, List(var0, var0))
      creating = false
    }
    if(var0 != null)
      add(Let(), n.deref, List(var0, n.deref))
  }
}