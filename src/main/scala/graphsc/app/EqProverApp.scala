package graphsc.app

import graphsc._
import interpretation._
import transformation._
import residualization._

import org.rogach.scallop._

object EqProverApp {
  
  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("Equivalence prover based on hypergraph supercompilation, version 0.0")
    banner("Usage: EqProver [OPTIONS] file")
    //val task = opt[String]()
    val arity = opt[Int](default = Some(3), descr = "Maximal arity of nodes")
    val depth = opt[Int](default = Some(3), descr = "Depth limit")
    val codepth = opt[Int](default = Some(3), descr = "Codepth limit")
    val nogen = opt[Boolean](noshort = true, descr = "Disable generalization")
    val generations = opt[Int](default = Some(1000), descr = "Maximal number of generations")
    val dumpDot = opt[Boolean](noshort = true, descr = "Dump the graph to stdout")
    val verbose = opt[Boolean](descr = "Be more verbose")
    val file = trailArg[String](required = true)
  }

  def main(args: Array[String]) {
    val conf = new Conf(args)
    val graph = new TheHypergraph
        with NamedNodes
        with Transformations
        with TransformManager 
        with DepthTracker
        with Prettifier 
        //with HyperTester
        //with HyperLogger
        //with IntegrityCheckEnabled
        //with OnTheFlyTesting
        with SelfLetAdder
    
    val maxarity = conf.arity.get.get
    val maxdepth = conf.depth.get.get
    val maxcodepth = conf.codepth.get.get
        
    // read the file
    val parser = ExprParser(graph)
    val src = io.Source.fromFile(conf.file.get.get)
    val srctext = src.mkString
    src.close()
    parser(srctext)
    
    // assign zero (co)depth to all initial nodes
    for(n <- graph.allNodes)
      graph.zeroBoth(n.deref)
      
    // This buffer stores all hyperedges that will be added to the graph
    val buf = HyperBuffer(graph)
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
    
    stats()
    
    // main loop
    while(!stop && generation < conf.generations.get.get) {
      while(graph.updatedHyperedges.nonEmpty) {
        val trans =
          if(conf.nogen.isSupplied) tr.transDrive
          else tr.transDrive & tr.letUp(maxarity)
        graph.transform(
            trans.cond(
                graph.limitDepth(maxdepth) & graph.limitCodepth(maxcodepth)).onSuccess(
                    () => { tr.commit(); } ))
      }
      buf.commit()
      
      generation += 1
      
      stats()
      
      
    }
    
    if(conf.dumpDot.isSupplied) {
      println(graph.toDot)
    }
  }

}