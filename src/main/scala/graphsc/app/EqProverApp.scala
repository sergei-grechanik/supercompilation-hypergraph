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
    val arity = opt[Int](default = Some(3), descr = "Maximal arity of nodes")
    val depth = opt[Int](default = Some(3), descr = "Depth limit")
    val codepth = opt[Int](default = Some(3), descr = "Codepth limit")
    val nogen = opt[Boolean](noshort = true, descr = "Disable generalization")
    val noiso = opt[Boolean](noshort = true, descr = "Disable merging by isomorphism")
    val generations = opt[Int](default = Some(1000), descr = "Maximal number of generations")
    val dumpDot = opt[Boolean](noshort = true, descr = "Dump the graph to stdout")
    val dumpCode = opt[Boolean](noshort = true, 
      descr = "Dump the graph to stdout in a form of a program")
    val dumpGenCode = opt[String](noshort = true, 
      descr = "Dump code for each generation to files <arg>-i")
    val verbose = opt[Boolean](descr = "Be more verbose")
    val file = trailArg[String](required = true)
    
    val nopretty = opt[Boolean](noshort = true, 
        descr="Disable transforming nodes to readable programs on the fly. " +
        		"Makes everything a bit faster.")
    val test = opt[Boolean](noshort = true, descr = "Enable testing on the fly")
    val integrityCheck = opt[Boolean](noshort = true, hidden = true)
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
        //with HyperLogger
        //with IntegrityCheckEnabled
        //with OnTheFlyTesting
        with SelfLetAdder
        with AutoTransformer {
          override val integrityCheckEnabled = conf.integrityCheck.isSupplied
          override val onTheFlyTesting = conf.test.isSupplied
          override val prettifyingEnabled = !conf.nopretty.isSupplied
        }
    
    val maxarity = conf.arity.get.get
    val maxdepth = conf.depth.get.get
    val maxcodepth = conf.codepth.get.get
    
    if(!conf.nogen.isSupplied)
      graph.autoTransformations ::= graph.unshare(maxarity)
        
    // read the file
    val parser = ExprParser(graph)
    val src = io.Source.fromFile(conf.file.get.get)
    val srctext = src.mkString
    src.close()
    parser(srctext)
    
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
      
    // This buffer stores all hyperedges that will be added to the graph
    val buf = graph//HyperBuffer(graph)
    // This buffer stores hyperedges for each transformation and makes sure
    // that no hyperedge exceeds the maximal arity
    val tr = new PostFilter(buf, h => h.arity <= maxarity) with Transformations
      
    
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
      graph.transform(
          trans.cond(
              graph.limitDepth(maxdepth) & graph.limitCodepth(maxcodepth)).onSuccess(
                  () => { tr.commit(); } ))
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
        for(((i,ren),l,r) <- like.toList.sortBy(-_._1._1)
              if i > 0 && l.deref.node != r.deref.node) {
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
            eq.get.performGluing(graph)
            val st = eprover.stats
            eprover = new EquivalenceProver(graph)
            eprover.stats = st
          }
        }
        
        /*
        val stats = eprover.stats
        
        for(((l1,r1),c) <- stats.toList.sortBy(_._2)) {
          val l = l1.deref.node
          val r = r1.deref.node
          val lik = LikenessCalculator[Int].likeness(l.deref, r.deref)
          if(lik != None && lik.get._1 == 0 ) {
            println(c + " $$$$$$$ This would help $$$$$$$$$ " + lik)
            println(l.prettyDebug)
            println("---")
            println(r.prettyDebug)
            println("^^^^^^^^^^")
            g.zeroBoth(l.deref)
            g.zeroBoth(r.deref)
            if(g.drive(l) == None)
              println("seems undrivable\n" + l.prettyDebug)
            if(g.drive(r) == None)
              println("seems undrivable\n" + r.prettyDebug)
          }
        }*/
      }
      
      stats()
      gendump()
      checktask()
      
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