import graphsc._
import app._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.ParallelTestExecution

@RunWith(classOf[JUnitRunner])
class EqProverAppSuite extends FunSuite with ParallelTestExecution {
  
  def cmd(s: String) {
    test("eqprover " + s) {
      assert(EqProverApp.mainBool(s.split(" ")) === Some(true))
    }
  }
  
  def cmdnot(s: String) {
    test("eqprover " + s) {
      assert(EqProverApp.mainBool(s.split(" ")) === Some(false))
    }
  }
  
  // small enough for generalization
  //cmd("--prove --integrity-check --test -a4 ./samples/add-assoc")
  cmd("--prove --integrity-check --test ./samples/dummy")
  cmd("--prove --integrity-check --test ./samples/even-double")
  cmd("--prove --integrity-check --test ./samples/idle")
  cmd("--prove --integrity-check --test ./samples/inf")
  cmd("--prove --integrity-check --test ./samples/map-comp")
  //cmd("--prove --integrity-check ./samples/quad-idle")
  
  cmd("--prove --integrity-check --test -a4 --nogen ./samples/add-assoc")
  cmd("--prove --integrity-check --test --nogen ./samples/idle")
  cmd("--prove --integrity-check --nogen ./samples/quad-idle")
  cmd("--prove --integrity-check --nogen ./samples/exp-idle")
  cmd("--prove --integrity-check --test --nogen ./samples/shuffled-let")
  cmd("--prove --integrity-check --test --nogen ./samples/bool-eq")
  cmd("--prove --integrity-check --test --nogen -a4 ./samples/small/case-swap")
  cmd("--prove --integrity-check --test --nogen ./samples/sadd-comm")
  cmd("--prove --integrity-check --test --nogen ./samples/idnat-idemp")
  //cmd("--prove -c10 -d10 -a10 --nogen -v ./samples/mul-distrib-and-assoc")
  
  // total
  cmd("--prove --integrity-check --test --nogen --total ./samples/total/construct-caseof")
  cmd("--prove --integrity-check --test --nogen --total ./samples/total/useless-caseof")
  cmd("--prove --integrity-check --test --nogen --total ./samples/total/add-comm-lemma")
  cmd("--prove --integrity-check --test --nogen --total ./samples/total/add-comm")
  
  // unprovable
  cmdnot("--prove --integrity-check --test --nogen --total ./samples/unprovable")
  cmdnot("--prove --integrity-check --test --nogen ./samples/unprovable")
  cmdnot("--prove --integrity-check --test --nogen ./samples/total/construct-caseof")
  cmdnot("--prove --integrity-check --test --nogen ./samples/total/useless-caseof")
  cmdnot("--prove --integrity-check --test --nogen ./samples/total/add-comm-lemma")
  cmdnot("--prove --integrity-check --test -a4 ./samples/small/let-bug")
  
}
