import graphsc._
import transformation._
import residualization._
import interpretation._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.ParallelTestExecution

@RunWith(classOf[JUnitRunner])
class BiTransformationSuite extends FunSuite with ParallelTestExecution {
  import Samples._
  
  def transform(
      g: BiTransformManager with DepthTracker with Transformations,
      maxdepth: Int = Int.MaxValue) {
    
    val transAll: List[BiTransformation] = {
      import graphsc.transformation._
      List(g.letVar, g.letLet, g.letCaseOf, g.letOther,
           g.caseVar, g.caseCase, g.caseTick)
        .map {
          case BiTransformation(run, pri, pre) =>
            val newrun: (Hyperedge, Hyperedge) => Unit =
              (h1, h2) => if(g.limitDepth(maxdepth)(h1, h2)) run(h1, h2)
            BiTransformation(newrun, pri, pre)
        }
    }
    
    g.updateAll()
    while(g.transform(transAll)) {}
  }
  
  def runTest(
      namecode: (String, String), 
      input: List[Value], 
      maxdepth: Int = Int.MaxValue, 
      residuate: Boolean = false) {
    val (name, code) = namecode
    info(name + ":")
    
    val g = 
      new TheHypergraph 
        with HyperTester
        with NamedNodes
        with BiTransformManager
        with Transformations
        with DepthTracker
        with IntegrityCheckEnabled
        with OnTheFlyTesting
        with Prettifier
        
    ProgramParser(g, code)
    val main = g(name)
    // I'm always forgetting to call this function
    g.updateDepth(main.node, 0)
    val res = g.runNode(main, input).value
    
    transform(g, maxdepth)
    val nodes1 = g.allNodes.size
    val hyperedges1 = g.allHyperedges.size
    
    // transform should be idempotent
    transform(g, maxdepth)
    assert(g.allNodes.size === nodes1)
    assert(g.allHyperedges.size === hyperedges1)
  
    info("maxdepth: " + maxdepth)
    info(nodes1 + " nodes, " + hyperedges1 + " hyperedges")
    
    if(residuate) {
//      val g_filtered = new TheHypergraph with IntegrityCheckEnabled with TransformManager
//      val tf = new TerminationFilter(g_filtered)
//      val Some(n_filtered) = tf(main)
//      assert(HyperRunner.run(n_filtered, input) === res)
//      
//      info("After ''residualization'' " + g_filtered.allNodes.size + 
//            " nodes, " + g_filtered.allHyperedges.size + " hyperedges")
    }
      
    info("")
  }
  
  test("These simple functions don't need depth limit to be transformed") {
    runTest(Samples("add"), List(2, 3), residuate = true)
    runTest(Samples("add3Left", "add"), List(2, 3, 1), residuate = true)
    runTest(Samples("add3Right", "add"), List(2, 3, 1), residuate = true)
    runTest(Samples("const"), List(2, 3), residuate = true)
    runTest(Samples("id"), List(3), residuate = true)
    runTest(Samples("constz"), List(3), residuate = true)
    runTest(Samples("padd"), List(2, 3), residuate = true)
  }
  
  test("mul") { runTest(Samples("mul", "add"), List(2, 3), 3) }
  test("pmul") { runTest(Samples("pmul", "padd"), List(2, 3), 3) }
  test("reverseTo") { runTest(Samples("reverseTo"), List(2, 3), 5) }
  test("nrev") { runTest(Samples("nrev", "add"), List(3), 5) }
  //test("fac") { runTest(Samples("fac", "mul", "add"), List(3), 5) }
  //test("fib") { runTest(Samples("fib", "add"), List(3), 5) }
  //test("ackermann") { runTest(Samples("ackermann"), List(2, 3), 5) }
  test("append") { runTest(Samples("append"), List(list(1,2,3), list(4,5)), 5) }
  test("nrevL") { runTest(Samples("nrevL", "append"), List(list(1,2,3)), 5) }
}