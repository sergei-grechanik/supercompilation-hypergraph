package graphsc.app

import graphsc._
import interpretation._
import transformation._
import residualization._

import org.rogach.scallop._

object EqProverApp {
  
  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("Equivalence prover based on hypergraph supercompilation, version 0.1")
    banner("Usage: EqProver [OPTIONS] file")
    
    val task = opt[String](descr = 
      "An equivalence we want to prove (up to renaming!) of the form foo=bar " +
    		"or auto to read the task from the first line of the file")
    val resid = opt[String](descr = "Residualize the specified function. ")
    
		val arity = opt[Int](default = Some(3), descr = "Maximal arity of nodes")
    val depth = opt[Int](default = Some(3), descr = "Depth limit")
    val codepth = opt[Int](default = Some(3), descr = "Codepth limit")
    val nogen = opt[Boolean](noshort = true, descr = "Disable generalization")
    val noiso = opt[Boolean](noshort = true, descr = "Disable merging by isomorphism")
    val generations = opt[Int](default = Some(1000), descr = "Maximal number of generations")
    val driveRecommended = opt[Int](noshort = true, default = Some(0), 
        descr = "Drive 2*<arg> recommended (by eqprover) nodes")
    
    val dumpDot = opt[Boolean](noshort = true, descr = "Dump the graph to stdout")
    val dumpCode = opt[Boolean](noshort = true, 
      descr = "Dump the graph to stdout in a form of a program")
    val dumpGenCode = opt[String](noshort = true, 
      descr = "Dump code for each generation to files <arg>-i")
    val verbose = opt[Boolean](descr = "Be more verbose")
    val log = opt[Boolean](descr = "Log transformations to stdout")
    val file = trailArg[String](required = true)
    
    val nopretty = opt[Boolean](noshort = true, 
        descr="Disable transforming nodes to readable programs on the fly. " +
        		"Makes everything a bit faster.")
    val test = opt[Boolean](noshort = true, descr = "Enable testing on the fly")
    val integrityCheck = opt[Boolean](noshort = true, hidden = true)
    
    val cheat = opt[Boolean](noshort = true,
        descr = "Tweak the supercompiler so that it can solve the nrev example")
  }
  
  def main(args: Array[String]) = mainBool(args) match {
    case Some(true) =>
      System.err.println("SUCCESS: The equivalence was successfully proved (up to renaming)")
    case Some(false) =>
      System.err.println("FAIL: I was unable to prove the equivalence")
    case _ =>
  }
  
  def mainBool(args: Array[String]): Option[Boolean] = {
    val conf = new Conf(args)
    val graph = new TheHypergraph
        with NamedNodes
        with Transformations
        with BiTransformManager 
        with DepthTracker
        with Prettifier 
        with HyperTester
        //with IntegrityCheckEnabled
        //with OnTheFlyTesting
        with SelfLetAdder
        with AutoTransformer
        with HyperLogger {
          override val integrityCheckEnabled = conf.integrityCheck.isSupplied
          override val onTheFlyTesting = conf.test.isSupplied
          override val prettifyingEnabled = !conf.nopretty.isSupplied
          
          override def filterUpdatedPairs(pairs: List[(Hyperedge, Hyperedge)]): 
              List[(Hyperedge, Hyperedge)] =
            pairs.filter { 
              case (h1,h2) =>
                nodesOf(h1,h2).map(depths(_)).max <= conf.depth.get.get &&
                nodesOf(h1,h2).map(codepths(_)).max <= conf.codepth.get.get
            }
          
          override def enableLogging = conf.log.isSupplied
          
          var residualizing = false
          override def limitFromMinCost(c: Int): Int =
            if(residualizing) c
            else super.limitFromMinCost(c)
        }
    
    val maxarity = conf.arity.get.get
    val maxdepth = conf.depth.get.get
    val maxcodepth = conf.codepth.get.get
    
    if(!conf.nogen.isSupplied)
      graph.autoTransformations ::= graph.unshare(maxarity)
        
    // read the file
    val src = io.Source.fromFile(conf.file.get.get)
    val srctext = src.mkString
    src.close()
    val prog = ProgramParser.parseProg(srctext).simplify
    prog.loadInto(graph)
    
    // get the task
    val task =
      conf.task.get.map{ s => 
        if(s == "auto") {
          val m = "--[\t ]*([^= \t\n\r\f]+)=([^= \t\n\r\f]+)".r.findPrefixMatchOf(srctext)
          if(!m.isDefined) {
            System.err.println("Cannot read a task from file")
            System.exit(1)
          }
          (graph(m.get.group(1)), graph(m.get.group(2))) 
        }
        else {
          val names = s.split("=")
          if(names.length != 2) {
            System.err.println("Invalid task format: " + s)
            System.exit(1)
          }
          (graph(names(0)), graph(names(1)))
        }
      }
    
    // assign zero (co)depth to all initial nodes
    for(n <- graph.allNodes)
      graph.zeroBoth(n.deref)
      
    // load tests
    if(conf.test.isSupplied)
      prog.loadTestsInto(graph)
      
    // This buffer stores all hyperedges that will be added to the graph
    val buf = graph//HyperBuffer(graph)
    // This buffer stores hyperedges for each transformation and makes sure
    // that no hyperedge exceeds the maximal arity
    val tr =
      if(conf.cheat.isSupplied)
        new PostFilter(buf, h => h.arity <= maxarity) with Transformations
      else
        new PostFilter(buf, h => h.used.size <= maxarity) with Transformations
      
    
    var generation = 0
    var stop = false
    
    def stats() {
      if(conf.verbose.isSupplied) {
        System.err.println("Generation: " + generation)
        System.err.println("Nodes: " + graph.allNodes.size)
        System.err.println("Hyperedges: " + graph.allHyperedges.size)
        System.err.println()
      }
    }
    
    def checktask() : Boolean = {
      for((l,r) <- task)
        if(l ~~ r) {
          stop = true
          return true
        }
      false
    }
    
    def gendump() {
      if(conf.dumpGenCode.isSupplied) {
        val out = 
          new java.io.PrintWriter(
            new java.io.File(conf.dumpGenCode.get.get + "-" + generation))
        try {
          out.write(graph.toProg)
        } finally { out.close() }
      }
    }
    
    stats()
    gendump()
    checktask()
    
    // main loop
    while(!stop && generation < conf.generations.get.get) {
      if(conf.verbose.isSupplied)
          System.err.println("Transforming...")
      
      val trans =
        if(conf.nogen.isSupplied) tr.transDrive
        else tr.transDrive & tr.letUp(maxarity)
      graph.transform(trans)
      //buf.commit()
      
      generation += 1
      
      checktask()
            
      if(!stop && !conf.noiso.isSupplied) {
        if(conf.verbose.isSupplied)
          System.err.println("Computing likeness...")
        val nodes = graph.allNodes.toList
        val like =
          for(l <- nodes; r <- nodes; if l != r && l.hashCode <= r.hashCode; 
              lkl <- LikenessCalculator[Int].likenessN(l, r); if lkl._1 > 0) yield {
            (lkl,l,r)
          }
        
        var eprover = new EquivalenceProver(graph)
        
        if(conf.verbose.isSupplied)
          System.err.println("Performing merging by isomorphism...")
        val candidates = 
          like.toList.sortBy(-_._1._1)
            .filter(p => p._1._1 > 0 && !(p._2 ~~ p._3))
        if(conf.verbose.isSupplied)
          System.err.println("Number of candidate pairs: " + candidates.size)
        for(((i,ren),l,r) <- candidates 
            if !stop && !(l ~~ r)) {
          val lpretty = l.prettyDebug
          val rpretty = r.prettyDebug
          val eq = eprover.prove(l.deref.node, r.deref.node)
          if(eq != None) {
            if(conf.verbose.isSupplied) {
              System.err.println("==These two are equal==")
              System.err.println(lpretty)
              System.err.println("=======================")
              System.err.println(rpretty)
              System.err.println("=======================\n")
              //System.err.println(eq)
            }
            graph.log("-- Proved by isomorphism")
            eq.get.toLog(graph)
            graph.log("")
            eq.get.performGluing(graph)
            val st = eprover.stats
            eprover = new EquivalenceProver(graph)
            eprover.stats = st
            checktask()
          }
        }
        
        if(!stop && conf.driveRecommended.get.get > 0) {
          if(conf.verbose.isSupplied)
              System.err.println("Driving recommended nodes")
          val stats = eprover.stats 
          for(n <- stats.toList.filter 
                { case ((l,r),_) => 
                    LikenessCalculator[Int].likeness(l.deref, r.deref).map(_._1) == Some(0) }
                .sortBy(-_._2).take(conf.driveRecommended.get.get)
                .flatMap(p => List(p._1._1, p._1._2)).map(_.deref.node).distinct) {
            graph.drive(n)
          }
        }
      }
      
      stats()
      gendump()
      checktask()
      
    }
    
    // residualization (by testing)
    if(conf.resid.isSupplied) {
      if(conf.verbose.isSupplied)
        System.err.println("Residualizing...")
      
      graph.residualizing = true
      
      //val trie = Trie.mkTrie(graph(conf.resid.get.get))
      //println(trie.dump(10))
        
      // Even if there are cached results, they are of no use to us
      graph.clearRunCache()
      prog.loadTestsInto(graph)
      
      if(conf.verbose.isSupplied)
        System.err.println("Done running tests...")
        
      val subgraphs = ByTestingResidualizer(graph)(graph(conf.resid.get.get).node)
      if(conf.verbose.isSupplied)
        System.err.println("Residual graphs count: " + subgraphs.size)
      if(subgraphs.nonEmpty) {
        val res = subgraphs.minBy(_.nodes.size)
        for((n,h) <- res.hyperedges 
            if !n.isVar && !n.definingHyperedge.exists(
                              h => h.label.isInstanceOf[Construct] && h.dests.isEmpty )) {
          println(graph.nodeFunName(h.source) + " =\n" +
              indent(graph.prettyHyperedge(h, graph.nodeFunName), "  ") + ";\n")
        }
      }
    }
    
    if(conf.dumpDot.isSupplied) {
      println(graph.toDot)
    }
    
    if(conf.dumpCode.isSupplied) {
      println(graph.toProg)
    }
    
    if(conf.task.isSupplied)
      Some(checktask())
    else
      None
  }

}