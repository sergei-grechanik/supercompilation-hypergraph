package graphsc
package residualization

case class UnorderedPair[T](_1: T, _2: T) {
	override def equals(o: Any) = o match {
		case that: UnorderedPair[T] => that._1 == _1 && that._2 == _2 || that._1 == _2 && that._2 == _1
		case _ => false
	}
	override def hashCode = _1.hashCode * _2.hashCode
}

case class HyperedgePairs(
            ins: collection.mutable.Set[(Hyperedge, Hyperedge)] = collection.mutable.Set[(Hyperedge, Hyperedge)](),
            outs: collection.mutable.Set[(Hyperedge, Hyperedge)] = collection.mutable.Set[(Hyperedge, Hyperedge)]())

class PairSubgraph(graph: TheHypergraph) {
  val nodePairs = collection.mutable.Map[UnorderedPair[Node], HyperedgePairs]()

  // remove (n1,n2) from the candidates, remove incident hyperedges, and recursively remove orphaned node pairs
  def remove(n1: Node, n2: Node): Unit = {
    nodePairs.remove(UnorderedPair(n1, n2)) match {
      case Some(hyperedge_pairs) =>
        for(pair <- hyperedge_pairs.ins.toList)
          removeHyperedgePair(pair)
        for(pair <- hyperedge_pairs.outs.toList)
          removeHyperedgePair(pair)
      case None =>
    }
  }

  def removeHyperedgePair(pair: (Hyperedge, Hyperedge)): Unit = {
    val (h1, h2) = pair
    for(src_hyper_pairs <- nodePairs.get(UnorderedPair(h1.source.node, h2.source.node))) {
      src_hyper_pairs.outs -= ((h1, h2))
      src_hyper_pairs.outs -= ((h2, h1))
      if(src_hyper_pairs.outs.isEmpty)
        remove(h1.source.node, h2.source.node)
    }
    for(dst_pair <- childrenByPairH((h1, h2)); dst_hyper_pairs <- nodePairs.get(dst_pair)) {
      dst_hyper_pairs.ins -= ((h1, h2))
      dst_hyper_pairs.ins -= ((h2, h1))
      if(dst_hyper_pairs.ins.isEmpty)
        remove(dst_pair._1, dst_pair._2)
    }
  }

  protected def childrenByPairH(pair: (Hyperedge, Hyperedge)): Set[UnorderedPair[Node]] =
    (pair._1.dests, pair._2.dests).zipped.map((d1, d2) => UnorderedPair(d1.node, d2.node)).toSet

  def checkConsistency(): Unit = {
    for((pair, hypers) <- nodePairs) {
      assert(nodePairs.contains(UnorderedPair(pair._2, pair._1)))
      for((o1, o2) <- hypers.outs; pred = UnorderedPair(o1.source.node, o2.source.node)) {
        assert(pred == pair)
        checkHyperedgePair((o1, o2))
      }
      for(ipair <- hypers.ins) {
        assert(childrenByPairH(ipair).contains(pair))
        checkHyperedgePair(ipair)
      }
    }
  }

  def checkHyperedgePair(pair: (Hyperedge, Hyperedge)): Unit = {
    for(succ <- childrenByPairH(pair)) {
      assert(succ._1 == succ._2 || nodePairs.contains(succ))
      if(succ._1 != succ._2)
        assert(nodePairs(succ).ins.contains(pair) || nodePairs(succ).ins.contains(pair.swap))
    }
    val pred = UnorderedPair(pair._1.source.node, pair._2.source.node)
    assert(nodePairs.contains(pred))
    assert(nodePairs(pred).outs.contains(pair) || nodePairs(pred).outs.contains(pair.swap))
  }
}

// Newer equivalence prover, builds the cartesian square of the graph
class EquivalenceProverAdvanced(graph: TheHypergraph with HyperLogger, likenesscalc: LikenessCalculator,
                                max_failures_per_pair: Int = 100) extends PairSubgraph(graph) {
  import CorrectnessChecker._

  println("Running equivalence prover...")
  println("Computing cartesian square graph...")

  for(n <- graph.nodes) {
    n.outsMut
    assert(n.isReal)
  }

  // add every noncontradictory pair of nodes to the map
  for(n1 <- graph.nodes; n2 <- graph.nodes
      if n1 != n2 && n1.used.size == n2.used.size &&
         n1.hashCode() <= n2.hashCode() && !contradictory(n1, n2) /*likenesscalc.likenessN(n1, n2).isDefined*/) {
    val pair = UnorderedPair(n1, n2)
    if(!nodePairs.contains(pair)) {
      nodePairs += pair -> HyperedgePairs()
    }
  }

  println("- added pairs")

  // for each pair add every possible pair of compatible hyperedges
  for((pair, hyperedge_pairs) <- nodePairs) {
    for(h1 <- pair._1.outsMut; h2 <- pair._2.outsMut; if h1.label == h2.label) h1.label match {
      case Id() =>
      case _ =>
        if(h1.dests.size == h2.dests.size) {
          val dest_pairs =
            for((d1, d2) <- (h1.dests, h2.dests).zipped) yield nodePairs.get(UnorderedPair(d1.node, d2.node)) match {
              case Some(hypers) => Some(Some(hypers))
              case None => if(d1.node == d2.node) Some(None) else None
            }
          if(dest_pairs.forall(_.isDefined)) {
            hyperedge_pairs.outs += ((h1, h2))
            for(Some(Some(hypers)) <- dest_pairs)
              hypers.ins += ((h1, h2))
          }
        }
    }
  }

  println("- added hyperedges")

//  checkConsistency()
//  println("CHECKED!!!")

  // call remove for orphan pairs
  for((UnorderedPair(n1, n2), hypers) <- nodePairs.toList
      if hypers.ins.isEmpty || hypers.outs.isEmpty) {
    remove(n1, n2)
//    checkConsistency()
  }

  println("- removed orphans")

  println("Cartesian square graph computed: " + nodePairs.size + " pairs out of " + scala.math.pow(graph.nodes.size,2))

  def mergeAll() {
    var zero_trees = 0
    println("Computing importance")
    val impmap = computeImportance()
    val sorted_list = nodePairs.keys.toList.map(k => (k, impmap(k))).sortBy(-_._2)
    // try to find locally safe trees for every pair
    for ((UnorderedPair(n1, n2), importance) <- sorted_list) {
      tryMerging(n1, n2, "importance: " + importance)
    }
    println("zero trees: " + zero_trees)
  }

  def tryMerging(n1: Node, n2: Node, additional_info: String = ""): Boolean = {
    if(!nodePairs.contains(UnorderedPair(n1, n2)))
      return false

    if(n1.deref.node == n2.deref.node) {
      // since other pairs may depent on this, we cannot delete this pair
//      for(pair <- nodePairs(UnorderedPair(n1, n2)).outs.toList)
//        removeHyperedgePair(pair)
      return false
    }

    println("@@@@@@@@ Trying to merge @@@@@@@@@@ " + additional_info)
    println(n1.prettyDebug)
    println("~~~~~~~~~~~~~~~~~~~~")
    println(n2.prettyDebug)
    println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

    val trees = locallySafeTrees(n1, n2)

    if(trees.isEmpty) {
      remove(n1, n2)
      println("Empty sequence\n")
      return false
    }

    var correctness_failures = 0
    var propagation_failures = 0

    for(tree <- trees) {
      try {
        val propagated = tree.propagateRenamings
        if (propagated.checkCorrectness) {
          propagated.performGluing(graph)
          println("=========== MERGED! =============\n")
          return true
        } else
          correctness_failures += 1
      } catch {
        case UncombinableRenamingsException =>
          propagation_failures += 1
      }
      if(correctness_failures + propagation_failures >= max_failures_per_pair) {
        println("Merging failed: unpropagatable " + propagation_failures +
            ", incorrect recursion " + correctness_failures + "\n")
        remove(n1, n2)
        return false
      }
    }
    println("Merging failed: unpropagatable " + propagation_failures +
            ", incorrect recursion " + correctness_failures + "\n")
    remove(n1, n2)
    return false
  }

  def computeImportance(): Map[UnorderedPair[Node], Int] = {
    val depmap = collection.mutable.Map[UnorderedPair[Node], collection.mutable.Set[UnorderedPair[Node]]]()
    for((pair, hypers) <- nodePairs) {
      val depset = depmap.getOrElseUpdate(pair, collection.mutable.Set(pair))
      for((i1, i2) <- hypers.ins; pred = UnorderedPair(i1.source.node, i2.source.node)) {
        if(nodePairs(pred).outs.forall(p => childrenByPairH(p).contains(pair)))
          depset += pred
      }
      for(opair <- hypers.outs; succ <- childrenByPairH(opair) if succ._1 != succ._2) {
        if(nodePairs(succ).ins.forall(p => UnorderedPair(p._1.source.node, p._2.source.node) == pair))
          depset += succ
      }
    }

    var changed = true
    while(changed) {
      changed = false
      for((pair, depset) <- depmap) {
        val size_before = depset.size
        for(dep <- depset.toList if dep != pair)
          depset ++= depmap(dep)
        if(size_before != depset.size)
          changed = true
      }
    }

    return depmap.mapValues(_.size).toMap
  }

  private def contradictory(n1: Node, n2: Node, maxdepth: Int = 3): Boolean = {
    val d1 = n1.definingHyperedge
    val d2 = n2.definingHyperedge
    if(d1.isDefined && d2.isDefined) {
      if(d1.get.label != d2.get.label)
        return true
      else if(maxdepth > 0) {
        return (d1.get.dests, d2.get.dests).zipped.exists(
                  (p: (RenamedNode, RenamedNode)) =>
                      contradictory(p._1.deref.node, p._2.deref.node, maxdepth - 1))
      }
    }
    return false
  }

  type S = RSMatrix
  type Hist = List[((Node, S), (Node, S))]
  type HistSet = Set[((Node, S), (Node, S))]

  def locallySafeTrees(
      l1: Node,
      r1: Node,
      ren1: Renaming = Renaming(),
      hist1: Hist = Nil): Stream[EqProofTree] = {

    val lind = hist1.indexWhere(_._1._1 == l1)
    val rind = hist1.indexWhere(_._2._1 == r1)

    if(l1 == r1)
      (ren1 | Renaming(r1.used)) match {
        case Some(newren) => Stream(EqProofTree(newren, (l1,r1)))
        case None => Stream()
      }
    else if(lind != -1 && rind != -1) {
      hist1.find(p => p._1._1 == l1 && p._2._1 == r1) match {
        case Some(((_, lsafety), (_, rsafety))) if lsafety.locallySafe && rsafety.locallySafe =>
          Stream(EqProofTree(ren1, (l1,r1)))
        case _ => Stream()
      }
    } else {
      val preouts = nodePairs(UnorderedPair(l1, r1)).outs.toList
      val outs =
        for((h1, h2) <- preouts) yield {
          assert(h1.source.node == l1 || h1.source.node == r1)
          assert(h2.source.node == l1 || h2.source.node == r1)
          val swap = h1.source.node != l1
          if(swap) (h2, h1) else (h1, h2)
        }

      locallySafeTreesByCong(l1, r1, outs, ren1, hist1)
    }
  }

  def locallySafeTreesByCong(
      l: Node,
      r: Node,
      outs: List[(Hyperedge, Hyperedge)],
      ren: Renaming = Renaming(),
      hist: Hist = Nil): Stream[EqProofTree] = {
    for ((lh, rh) <- outs.toStream; tree <- locallySafeTreesByCongH(l, r, lh, rh, ren, hist))
      yield tree
  }

  def locallySafeTreesByCongH(
      l: Node, r: Node,
      lh: Hyperedge, rh: Hyperedge,
      ren: Renaming = Renaming(),
      hist: Hist = Nil): Stream[EqProofTree] = {

    val lh_matrices = hyperedgeToMatrix(lh)
    val rh_matrices = hyperedgeToMatrix(rh)

    val newhisthead = ((l, idMatrix(RenamedNode.fromNode(l))), (r, idMatrix(RenamedNode.fromNode(r))))
    val newss1 =
      (newhisthead :: hist).map(p => lh_matrices.map(_ * p._1._2)).transpose
    val newss2 =
      (newhisthead :: hist).map(p => rh_matrices.map(_ * p._2._2)).transpose

    val lhs = lh.dests zip newss1
    val rhs = rh.dests zip newss2

    val pushed_rens = EquivalenceProver.pushRenamingThroughHyperedges(ren, lh, rh)

    val dest_trees =
      for(((ld, news1), (rd, news2), dren) <- (lhs, rhs, pushed_rens).zipped) yield {
        val newhist =
          ((newhisthead :: hist) zip (news1 zip news2)).map {
            case (((n1, _), (n2, _)), (s1, s2)) =>
              ((n1, s1), (n2, s2))
          }

        locallySafeTrees(ld.node, rd.node, dren, newhist)
      }

    sequenceS(dest_trees.toList).map(dests => EqProofTree(ren, (l,r), Some((lh, rh, dests))))
  }

}