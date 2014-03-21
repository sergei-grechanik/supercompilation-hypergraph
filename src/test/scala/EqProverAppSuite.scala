import graphsc._
import app._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.ParallelTestExecution
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
class EqProverAppSuite extends FunSuite with ParallelTestExecution {
  
  def cmd(s: String, secs: Int = 30, res: Boolean = true) {
    test("eqprover " + s) {
      assert(Await.result(future(MainApp.mainBool(s.split(" "))), secs.seconds) === Some(res))
    }
  }
  
  def cmdnot(s: String, secs: Int = 30) {
    cmd(s, secs, false)
  }
  
  // small enough for generalization
  cmd("--prove --integrity-check --test --gen -a4 ./samples/add-assoc")
  cmd("--prove --integrity-check --test --gen ./samples/dummy")
  cmd("--prove --integrity-check --test --gen ./samples/even-double")
  cmd("--prove --integrity-check --test --gen ./samples/idle")
  cmd("--prove --integrity-check --test --gen ./samples/inf")
  cmd("--prove --integrity-check --test --gen ./samples/map-comp")
  //cmd("--prove --integrity-check --test ./samples/quad-idle")
  
  cmd("--prove --integrity-check --test -a4 ./samples/add-assoc")
  cmd("--prove --integrity-check --test ./samples/idle")
  cmd("--prove --integrity-check ./samples/quad-idle")
  cmd("--prove --integrity-check ./samples/exp-idle")
  cmd("--prove --integrity-check --test ./samples/shuffled-let")
  cmd("--prove --integrity-check --test ./samples/bool-eq")
  cmd("--prove --integrity-check --test -a4 ./samples/small/case-swap")
  cmd("--prove --integrity-check --test ./samples/sadd-comm")
  cmd("--prove --integrity-check --test ./samples/idnat-idemp")
  cmd("--prove -c10 -d10 -a10 ./samples/mul-distrib-and-assoc", 60)
  cmd("--prove -c10 -d10 -a10 ./samples/mul-distrib", 60)
  cmd("--prove -c10 -d10 -a10 ./samples/mul-assoc", 40)
  cmd("--prove --test -c10 -d10 -a10 ./samples/or-even-odd")
  cmd("--prove --test -c10 -d10 -a10 ./samples/orElse-even-odd")
  cmd("--prove --test -c10 -d10 -a10 ./samples/orElseT-even-odd")
  cmd("--prove --integrity-check samples/shifted-cycle")
  
  // total
  cmd("--prove --integrity-check --test --total ./samples/total/construct-caseof")
  cmd("--prove --integrity-check --test --total ./samples/total/useless-caseof")
  cmd("--prove --integrity-check --test --total ./samples/total/add-comm-lemma")
  cmd("--prove --integrity-check --test --total ./samples/total/add-comm")
  
  // unprovable
  cmdnot("--prove --integrity-check --test --total ./samples/unprovable")
  cmdnot("--prove --integrity-check --test ./samples/unprovable")
  cmdnot("--prove --integrity-check --test ./samples/total/construct-caseof")
  cmdnot("--prove --integrity-check --test ./samples/total/useless-caseof")
  cmdnot("--prove --integrity-check --test ./samples/total/add-comm-lemma")
  cmdnot("--prove --integrity-check --test --gen -a4 ./samples/small/let-bug")
  
  // rules
  cmd("--prove --integrity-check --test --total samples/rules/arith-square-of-sum-test")
  cmd("--prove --integrity-check samples/rules/arith-square-of-sum")
  cmd("--prove --integrity-check --test samples/rules/nrev-nat-1")
  cmd("--prove samples/rules/even-dbl-acc-lemma-1")
  cmd("--prove samples/rules/map-iterate")
  
  // correctness checker
  cmd("--prove --integrity-check samples/correctness/ackermann")
  cmd("--prove --integrity-check samples/correctness/zip")
  cmd("--prove --integrity-check samples/correctness/fading-pulse")
  cmdnot("--prove --integrity-check samples/correctness/eatswap")
  cmdnot("--prove --integrity-check -g5 samples/correctness/nonterminating")
  cmdnot("--prove --integrity-check samples/correctness/chpred-bug")
}
