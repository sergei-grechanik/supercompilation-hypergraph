import graphsc._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class HyperTesterSuite extends FunSuite {
  import Samples._
  
  val samples = 
    List(
      "const", "add", "mul", "padd", "pmul", "id", "nrev", 
      "fac", "fib", "append", "nrevL", "ackermann")
  
  def addSamples(g: NamedNodes) {
    val parser = new ExprParser(g)
    for(s <- samples)
      parser(Samples(s))
  }
  
  def runSamples(g: HyperTester with NamedNodes) {
    assert(g.runNode(g("const"), List(2, 3)) === peano(2))
    assert(g.runNode(g("add"), List(2, 3)) === peano(5))
    assert(g.runNode(g("add"), List(5, 1)) === peano(6))
    assert(g.runNode(g("mul"), List(2, 3)) === peano(6))
    assert(g.runNode(g("mul"), List(4, 3)) === peano(12))
    assert(g.runNode(g("padd"), List(2, 3)) === peano(5))
    assert(g.runNode(g("pmul"), List(2, 3)) === peano(6))
    assert(g.runNode(g("id"), List(3)) === peano(3))
    assert(g.runNode(g("nrev"), List(2)) === peano(2))
    assert(g.runNode(g("nrev"), List(5)) === peano(5))
    assert(g.runNode(g("fac"), List(4)) === peano(24))
    assert(g.runNode(g("fib"), List(6)) === peano(8))
    assert(g.runNode(g("nrevL"), List(list(1,2,3,4))) === list(4,3,2,1))
    // HyperTester has a depth limit, so we cannot use too large arguments
    assert(g.runNode(g("ackermann"), List(2, 3)) === peano(9))
    assert(g.runNode(g("ackermann"), List(3, 2)) === peano(29))
  }
  
  test("Evaluation with HyperTester") {
    val g = 
      new TheHypergraph 
        with HyperTester
        with NamedNodes
        with IntegrityCheckEnabled
    
    addSamples(g)
    runSamples(g)
  }
}
