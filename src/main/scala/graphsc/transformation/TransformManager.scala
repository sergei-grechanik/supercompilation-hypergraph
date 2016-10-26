package graphsc
package transformation
import scala.util.Random

trait BiTransformManager extends TheHypergraph with DepthTracker {
  var totalChanges: Int = 0
  var changed = true
  var lastPairsProcessed = 0
  val potentialTransformations = collection.mutable.Set[PotentialTransformation]()
  val updatedPairs = collection.mutable.Set[(Hyperedge, Hyperedge)]()
  
  // TODO: Find a better name
  def updateAll() {
    for(n <- allNodes; h1 <- n.insMut; h2 <- n.outsMut)
      addPair(h1, h2)    
  }
  
  private def addPair(h1: Hyperedge, h2: Hyperedge) {
    //log("Pair:\n" + hyperedgeToString(h1) + "\n" + hyperedgeToString(h2) + "\n\n")
    updatedPairs.add((h1, h2))
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    totalChanges += 1
    changed = true
    for(h1 <- h.source.node.insMut)
      addPair(h1, h)
    for(d <- h.dests; h2 <- d.node.outsMut)
      addPair(h, h2)
    super.onNewHyperedge(h)
  }
  
  override def beforeGlue(r: RenamedNode, n: Node) {
    totalChanges += 1
    changed = true
    for(h1 <- n.insMut; h2 <- r.node.outsMut)
      addPair(h1, h2)
    for(h1 <- r.node.insMut; h2 <- n.outsMut)
      addPair(h1, h2)
    super.beforeGlue(r, n)
  }
  
  override def onUsedReduced(n: Node) {
    totalChanges += 1
    changed = true
    for(h1 <- n.insMut; h2 <- n.outsMut)
      addPair(h1, h2) 
    super.onUsedReduced(n)
  }

  def enqueueTransformations(
          trans: Traversable[BiTransformation], 
          pair_filter: ((Hyperedge, Hyperedge)) => Boolean = _ => true,
          destructive: Boolean = true) {
    val normalized = updatedPairs.map(p => (normalize(p._1), normalize(p._2)))
    updatedPairs.clear()
    updatedPairs ++= normalized
    val set = updatedPairs.toList.filter(pair_filter)
    if(destructive)
      updatedPairs --= set
    val processed = collection.mutable.Set[(Hyperedge, Hyperedge)]()
    for((h1, h2) <- set) {
      potentialTransformations ++= 
          trans.collect { case t if t.prefilter(h1, h2) => PotentialTransformation(h1, h2, t) }
    }
  }

  def simplifyPotentialTransformations() {
    val new_pt: List[PotentialTransformation] =
      (for(PotentialTransformation(h1o, h2o, tr) <- potentialTransformations.toTraversable;
          h1 = normalize(h1o);
          h2 = normalize(h2o);
          if h1.source.node.outsMut(h1) && h2.source.node.outsMut(h2);
          if tr.prefilter(h1, h2))
        yield PotentialTransformation(h1, h2, tr)).toList
    println("Potential transformations before simplification: " + potentialTransformations.size)
    potentialTransformations.clear()
    potentialTransformations ++= new_pt
    println("Potential transformations after simplification: " + potentialTransformations.size)
  }

  def runPotentialTransformations(maxtrans: Int, ignore_nonchanging: Boolean = true) {
    println("Potential transformations before running: " + potentialTransformations.size)
    println("Nodes before: " + this.nodes.size)
    var count = 0
    var ignored = 0
    var lastprio: Double = 0
    for((pr, pt) <- potentialTransformations.toList
                    .map(p => (p.trans.priority_function(normalize(p.h1), normalize(p.h2)), p))
                    .sortBy(_._1) if count < maxtrans) {
      potentialTransformations -= pt
      val h1 = normalize(pt.h1);
      val h2 = normalize(pt.h2);
      if(h1.source.node.outsMut(h1) && h2.source.node.outsMut(h2)) {
        val changes_before = totalChanges
        pt.trans.run(h1, h2)
        if(!ignore_nonchanging || changes_before != totalChanges) {
          count += 1
          lastprio = pr
        }
        else
          ignored += 1
      }
    }
    println("Potential transformations after running: " + potentialTransformations.size)
    println("Ignored transformations: " + ignored + " last priority: " + lastprio)
    println("Nodes after: " + this.nodes.size)
  }

  // obsolete
  def transform(trans: Traversable[BiTransformation], 
      pair_filter: ((Hyperedge, Hyperedge)) => Boolean = _ => true,
      destructive: Boolean = true): Boolean = {
    val normalized = updatedPairs.map(p => (normalize(p._1), normalize(p._2)))
    updatedPairs.clear()
    updatedPairs ++= normalized
    //println("Pairs to transform before filtering: " + updatedPairs.size)
    val set = updatedPairs.toList.filter(pair_filter)
    if(destructive)
      updatedPairs --= set
    //println("Pairs to transform: " + set.size)
    var count = 0
    val processed = collection.mutable.Set[(Hyperedge, Hyperedge)]()
    for((h1o, h2o) <- set) {
      val h1 = normalize(h1o)
      val h2 = normalize(h2o)
      if(!processed((h1,h2)) && !processed((h1o,h2o)) 
          && h1.source.node.outsMut(h1) && h2.source.node.outsMut(h2)) {
        processed.add((h1, h2))
        for(p <- trans)
          p.run(h1, h2)
        count += 1
      }
    }
    lastPairsProcessed = count
    //println("Pairs transformed: " + count)
    updatedPairs.nonEmpty
  }

}

