package graphsc

trait HyperLogger extends Prettifier {
  def enableLogging = true
  var logshift = 0
  
  override def log(s: String) {
    if(enableLogging) {
      val ind = List.fill(logshift)("  ").mkString("")
      println(ind + s.replace("\n", "\n" + ind))
    }
  }
  
  override def nodeToString(n: RenamedNode): String =
    if(enableLogging)
      nodeShortProg(n)
    else
      "<node>"
  
  override def hyperedgeToString(h: Hyperedge): String =
    if(enableLogging)
      nodeToString(h.source) + " = " + prettyHyperedge(h, nodeToString, true).replace("\n", " ")
    else
      "<hyperedge>"
  
  override def logShift() { logshift += 1 }
  override def logUnshift() { logshift -= 1 }
    
  override def addHyperedge(h: Hyperedge) {
    if(enableLogging) {
      log("-- addHyperedge " + h.label)
      log("-- " + hyperedgeToString(h))
      log("")
    }
    logShift()
    super.addHyperedge(h)
    logUnshift()
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    if(enableLogging) {
      log("-- onNewHyperedge " + h.label)
      log(hyperedgeToString(h))
      log("")
    }
    super.onNewHyperedge(h)
  }
  
  override def newNode(a: Set[Int]): RenamedNode = {
    val n = super.newNode(a)
    if(enableLogging) {
      log("-- newNode: " + nodeToString(n))
      log("")
    }
    n
  }
  
  override def beforeGlue(l: RenamedNode, r: Node) {
    if(enableLogging) {
      log("-- beforeGlue")
      log(nodeToString(l) + " = " + nodeToString(r.deref))
      log("-- reglued hyperedges:")
      for(h <- r.ins ++ r.outs)
        log(hyperedgeToString(normalize(h.replace(r, l))))
      log("")
    }
    super.beforeGlue(l, r)
  }
  
  override def afterGlue(n: Node, r: Node) {
    if(enableLogging) {
      log("-- glued: " + nodeToString(n.deref))
      log("")
    }
    super.afterGlue(n, r)
  }
  
  override def onUsedReduced(n: Node) {
    if(enableLogging) {
      log("-- used reduced: " + nodeToString(n.deref))
      log("")
    }
    super.onUsedReduced(n)
  }
  
  override def onNameChanged(n: Node, oldname: String) {
    if(enableLogging) {
      log("-- name changed")
      log(nodeToString(n.deref) + " = " + oldname)
      try {
        log(indent(pretty(n), "-- "))
      } catch {case _:NoSuchElementException => }
      log("-- renamed hyperedges:")
      for(h <- n.ins ++ n.outs)
        log(hyperedgeToString(h))
      log("")
    }
    super.onNameChanged(n, oldname)
  }
}