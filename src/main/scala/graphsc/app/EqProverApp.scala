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

    val total = opt[Boolean](descr = "Assume that we work in total setting")
    
    val prove = opt[Boolean](descr = 
      "Prove the propositions specified in the input file and then exit")
    
    val resid = opt[Boolean](descr = "Residualize the function specified in the input file")
    val residAutoTest = opt[Int](noshort = true, descr = 
      "Run <arg> smallest automatically generated tests for each node on the defining boundary")
    val residAutoTestOnly = opt[Boolean](noshort = true, descr =
      "Don't use user-specified tests for residualization")
      
    val only = opt[Boolean](descr = "Don't load unreferenced functions into graph")
		
    val arity = opt[Int](default = Some(3), descr = "Maximal arity of nodes")
    val depth = opt[Int](default = Some(3), descr = "Depth limit")
    val codepth = opt[Int](default = Some(3), descr = "Codepth limit")
    val nogen = opt[Boolean](noshort = true, descr = "Disable unordered generalization")
    val noiso = opt[Boolean](noshort = true, descr = "Disable merging by isomorphism")
    val generations = opt[Int](default = Some(1000), descr = "Maximal number of generations")
    val driveRecommended = opt[Int](noshort = true, default = Some(0), 
        descr = "Drive 2*<arg> recommended (by eqprover) nodes")
    val weakMerging = opt[Boolean](noshort = true, descr = "Disable merging up to renaming")
    val genPair = opt[Boolean](noshort = true, 
        descr = "Traditional pairwise generalization")
    val supercompile = opt[Boolean](name = "super", 
        descr = "Traditional multiresult supercompilation")
    
    val dumpDot = opt[Boolean](noshort = true, descr = "Dump the graph to stdout")
    val dumpCode = opt[Boolean](noshort = true, 
      descr = "Dump the graph to stdout in a form of a program")
    val dumpGenCode = opt[String](noshort = true, 
      descr = "Dump code for each generation to files <arg>-i")
    val verbose = opt[Boolean](descr = "Be more verbose")
    val log = opt[Boolean](descr = "Log transformations to stdout")
    val stat = opt[Boolean](descr = "Print some statistics like number of nodes at the end")
    
    val file = trailArg[String](required = true)
    
    val nopretty = opt[Boolean](noshort = true, 
        descr="Disable transforming nodes to readable programs on the fly. " +
        		"Makes everything a bit faster.")
    val test = opt[Boolean](noshort = true, descr = 
      "Enable testing on the fly using the tests specified in the input file")
    val testLog = opt[Boolean](noshort = true, descr = 
      "Enable tester logging")
    val integrityCheck = opt[Boolean](noshort = true, hidden = true)
    
    val cheat = opt[Boolean](noshort = true,
        descr = "Tweak the supercompiler so that it can solve the nrev example")
  }
  
  def main(args: Array[String]) = mainBool(args) match {
    case Some(true) =>
      System.err.println("SUCCESS: All the goals were successfully proved")
    case Some(false) =>
      System.err.println("FAIL: I was unable to prove some of the goals")
      System.exit(1)
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
          override val total = conf.total()
          override val integrityCheckEnabled = conf.integrityCheck()
          override val onTheFlyTesting = conf.test()
          override val prettifyingEnabled = !conf.nopretty()
          override val weakMerging = conf.weakMerging()
          
          override def filterUpdatedPairs(pairs: List[(Hyperedge, Hyperedge)]): 
              List[(Hyperedge, Hyperedge)] =
            pairs.filter { 
              case (h1,h2) =>
                nodesOf(h1,h2).map(depths(_)).max <= conf.depth() &&
                nodesOf(h1,h2).map(codepths(_)).max <= conf.codepth()
            }
          
          var enableLoggingVar = conf.log()
          override def enableLogging = enableLoggingVar
          override val hyperTesterLogging = conf.testLog()
          
          
          var residualizing = false
          override def limitFromMinCost(c: Int): Int =
            if(residualizing) c
            else super.limitFromMinCost(c)
        }
    
    val maxarity = conf.arity()
    val maxdepth = conf.depth()
    val maxcodepth = conf.codepth()
    
    if(!conf.nogen())
      graph.autoTransformations ::= graph.unshare(maxarity)
        
    // read the file
    val preprog = ProgramParser.parseFile(conf.file()).resolveUnbound
    val prog = if(conf.only()) preprog.removeUnreferenced.simplify else preprog.simplify
    val propdefs = collection.mutable.Set(prog.propdefs.toSeq:_*)
    prog.loadInto(graph)
    
    // load props we want to prove as goals
    val goals =
      if(conf.prove())
        prog.prove.map(_.loadAsGoal(graph))
      else Nil
      
    if(conf.prove() && goals.isEmpty) {
      System.err.println("--prove is supplied but there is nothing to prove")
      System.exit(2)
    }
      
    val resid =
      if(conf.resid())
        prog.residualize.map(_.bindUnbound.loadInto(graph))
      else Nil
    
    // assign zero (co)depth to all initial nodes
    for(n <- graph.allNodes)
      graph.zeroBoth(n.deref)
      
    // load tests
    if(conf.test())
      prog.loadTestsInto(graph)
      
    // This buffer stores all hyperedges that will be added to the graph
    val buf = graph//HyperBuffer(graph)
    // This buffer stores hyperedges for each transformation and makes sure
    // that no hyperedge exceeds the maximal arity
    val tr =
      if(conf.cheat())
        new PostFilter(buf, h => h.arity <= maxarity) with Transformations
      else
        new PostFilter(buf, h => h.used.size <= maxarity) with Transformations
      
    
    var generation = 0
    var stop = false
    
    def stats() {
      if(conf.verbose()) {
        System.err.println("Generation: " + generation)
        System.err.println("Nodes: " + graph.allNodes.size)
        System.err.println("Hyperedges: " + graph.allHyperedges.size)
        System.err.println()
      }
    }
    
    def checktask() : Boolean = {
      for((name, p) <- propdefs.toList) {
        if(p.checkWithoutLoading(graph) && conf.verbose()) {
          System.err.println("!!! prop " + name + ": " + p)
          propdefs.remove((name, p))
        }
      }
      
      val achieved =
        for(g <- goals) yield g match {
          case GoalPropEqModuloRen(l, r) => l ~~ r
          case GoalPropEq(l, r) => l ~=~ r
          case _ => false
        }
      if(achieved.nonEmpty && achieved.forall(_ == true)) {
        stop = true
        true
      } else false
    }
    
    def gendump() {
      if(conf.dumpGenCode.isSupplied) {
        val out = 
          new java.io.PrintWriter(
            new java.io.File(conf.dumpGenCode() + "-" + generation))
        try {
          out.write(graph.toProg)
        } finally { out.close() }
      }
    }
    
    stats()
    gendump()
    checktask()
    
    // Traditional multiresult supercompilation
    if(conf.supercompile()) {
      if(conf.verbose())
        System.err.println("Supercompiling...")
        
      val s = new Supercompilation(graph, 5)
      val ns = graph.namedNodes.values.toList
      val fromgoals =
        for(g <- goals) yield g match {
          case GoalPropEqModuloRen(l, r) => List(l, r)
          case GoalPropEq(l, r) => List(l, r)
          case GoalPropReturnsConstr(l, _) => List(l)
        }
      
      for(r <- fromgoals.flatten ++ resid)
        s.supercompile(r.node)
        
      stats()
      gendump()
      checktask()
    }
    
    // main loop
    while(!stop && generation < conf.generations()) {
      if(conf.verbose())
        System.err.println("Transforming...")
      
      graph.changed = false
          
      val trans =
        (if(conf.nogen()) tr.transNone else partFun2BiHProc(tr.letUp(maxarity))) &
        (if(conf.total()) tr.transTotal else tr.transNone) &
        tr.transDrive
      if(!conf.supercompile())
        graph.transform(trans)
      //buf.commit()
      
      
      // Pairwise generalization
      if(conf.genPair()) {
        val genr = new Generalizer()
        val gens =
          for(i <- graph.nodes.toList; j <- graph.nodes.toList; 
              if i.hashCode() < j.hashCode()) yield
            genr.generalizeNodes(i, j)
        
        val gs = gens.flatten.sortBy(-_.depth).filter(_.depth > 1)
        for(g <- gs) {
          graph.log("-- Pairwise generalization")
          g.toLog(graph)
          graph.log("")
          g.performGeneralization(graph)
        }
      }
      
      generation += 1
      
      checktask()
          
      if(!stop && !conf.noiso()) {
        if(conf.verbose())
          System.err.println("Computing likeness...")
        val nodes = graph.allNodes.toList
        val likenesscalc = LikenessCalculator[Int](true)//conf.total())
        val like =
          for(l <- nodes; r <- nodes; if l != r && l.hashCode <= r.hashCode; 
              lkl <- likenesscalc.likenessN(l, r); if lkl._1 > 0) yield {
            (lkl,l,r)
          }
        
        var eprover = new EquivalenceProver(graph, likenesscalc)
        
        if(conf.verbose())
          System.err.println("Performing merging by isomorphism...")
        val candidates = 
          like.toList.sortBy(-_._1._1)
            .filter(p => p._1._1 > 0 && !(p._2 ~~ p._3))
        if(conf.verbose())
          System.err.println("Number of candidate pairs: " + candidates.size)
          
        for(((i,ren2),l1,r1) <- candidates; if !stop) {
          val lderef = l1.deref;
          val rderef = r1.deref;
          val l = lderef.node;
          val r = rderef.node;
          val ren1 = lderef.renaming.inv comp ren2 comp rderef.renaming;
          
          for(ren <- if(!(l ~~ r)) List(ren1) else likenesscalc.viablePermutations(l)) {
            val lpretty = l.prettyDebug
            val rpretty = r.prettyDebug
            val eq = eprover.prove(l.deref.node, r.deref.node, ren)
            if(eq != None) {
              if(conf.verbose()) {
                System.err.println("==These two are equal==")
                System.err.println(lpretty)
                System.err.println("=======================" + eq.get.renaming)
                System.err.println(rpretty)
                System.err.println("=======================\n")
                //System.err.println(eq)
              }
              graph.log("-- Proved by isomorphism")
              eq.get.toLog(graph)
              graph.log("")
              eq.get.performGluing(graph)
              val st = eprover.stats
              eprover = new EquivalenceProver(graph, likenesscalc)
              eprover.stats = st
              checktask()
            }
          }
        }
        
        if(!stop && conf.driveRecommended() > 0) {
          if(conf.verbose())
              System.err.println("Driving recommended nodes")
          val stats = eprover.stats 
          for(n <- stats.toList.filter 
                { case ((l,r),_) => 
                    LikenessCalculator[Int](conf.total())
                      .likeness(l.deref, r.deref).map(_._1) == Some(0) }
                .sortBy(-_._2).take(conf.driveRecommended())
                .flatMap(p => List(p._1._1, p._1._2)).map(_.deref.node).distinct) {
            graph.drive(n)
          }
        }
      }
      
      stats()
      gendump()
      checktask()
      
      if(!graph.changed)
        stop = true
    }
    
    // residualization (by testing)
    if(conf.resid()) {
      val residlist =
        if(resid.nonEmpty) resid.map(_.deref)
        else graph.namedNodes.values.toList.map(_.deref)
      
      if(conf.verbose())
        System.err.println("Residualizing...")
      
      graph.residualizing = true
        
      // Even if there are cached results, they are of no use to us
      graph.clearRunCache()
      
      if(!conf.residAutoTestOnly())
        prog.loadTestsInto(graph)
      
      val residualizer = ByTestingResidualizer(graph, conf.residAutoTest.get.getOrElse(1)) 
      
      if(conf.residAutoTest.isSupplied) {
        if(conf.verbose())
          System.err.println("Autogenerating and running tests...")
        for(n <- residlist)
          residualizer.autoTest(n.node)
      }
      
      if(conf.verbose())
        System.err.println("Done running tests...")
        
      val subgraphs = residualizer(residlist)
      if(conf.verbose())
        System.err.println("Residual graphs count: " + subgraphs.size)
      if(subgraphs.nonEmpty) {
        val res = subgraphs.minBy(_.nodes.size)
        for((n,h) <- res.hyperedges 
            if !n.isVar && !n.definingHyperedge.exists(
                              h => h.label.isInstanceOf[Construct] && h.dests.isEmpty )) {
          println(graph.nodeFunName(h.source) + " =\n" +
              indent(graph.prettyHyperedge(h, graph.nodeFunName), "  ") + ";\n")
        }
        for((name,n) <- graph.namedNodes if res.nodes.contains(n.deref.node)) {
          val dern = n.deref
          val r = graph.nodeFunName(dern)
          val l = name + graph.prettyRename(dern.renaming,
                      (0 until dern.node.arity).map("v" + _ + "v").mkString(" ", " ", ""))
          if(l != r)
            println(l + " = " + r + ";")
        }
      }
    }
    
    if(conf.dumpDot()) {
      println(graph.toDot)
    }
    
    if(conf.dumpCode()) {
      println(graph.toProg)
    }
    
    if(conf.stat()) {
      println("#nodes " + graph.allNodes.size)
      println("#hyperedges " + graph.allHyperedges.size)
    }
    
    if(conf.prove())
      Some(checktask())
    else
      None
  }

}