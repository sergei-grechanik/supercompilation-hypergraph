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
  
  cmd("--prove --integrity-check --test -a4 ./samples/add-assoc")
  cmd("--prove --integrity-check --test ./samples/dummy")
  cmd("--prove --integrity-check --test ./samples/even-double")
  cmd("--prove --integrity-check --test ./samples/idle")
  cmd("--prove --integrity-check --test --nogen ./samples/idle")
  // --test fails on quad-idle, --integrity-check takes too long
  cmd("--prove ./samples/quad-idle")
  cmd("--prove --integrity-check --nogen ./samples/quad-idle")
  // takes too long without --nogen
  cmd("--prove --integrity-check --nogen ./samples/exp-idle")
  cmd("--prove --integrity-check --test ./samples/inf")
  cmd("--prove --integrity-check --test ./samples/map-comp")
  cmd("--prove --integrity-check --test --nogen ./samples/shuffled-let")
  
}
