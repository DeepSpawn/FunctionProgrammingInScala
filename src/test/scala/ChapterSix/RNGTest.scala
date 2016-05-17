package ChapterSix

import ChapterSix.RNG._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, ShouldMatchers}
/**
  * Created by gtaylor on 6/04/2016.
  */
class RNGTest extends PropSpec with PropertyChecks with ShouldMatchers {

  property("nonNegativeInt only produced positive numbers") {
    forAll(minSuccessful(1000000)) { (n: Long) =>
      whenever(true) {
        nonNegativeInt(Simple(n))._1 >= 0
      }
    }
  }

  property("double produces numbers in the interval [0,1)") {
    forAll(minSuccessful(1000000)) { (n: Long) =>
      whenever(true){
        val rndD = double(Simple(n))._1
        rndD >= 0 && rndD < 1
      }
    }
  }

  property("double (map impl) produces numbers in the interval [0,1)") {
    forAll(minSuccessful(1000000)) { (n: Long) =>
      whenever(true){
        val rng = Simple(n)
        val rndD = doubleMap(rng)(rng)._1
        rndD >= 0 && rndD < 1
      }
    }
  }

  property("ints (using seq) produces a list of number of the correct length") {
    forAll(Gen.chooseNum(0, 1000)) { (n: Int) =>
      whenever(n > 0 && n < 1000) {
        val rng = Simple(n)
        val rands = ints(n)(rng)._1
        rands.length == n && rands.forall(num => n > 0 && n <= Integer.MAX_VALUE)
      }
    }
  }

  property("nonNegativeLessThanFlatmappy produces positive numbers in the right range numbers") {
    forAll(Gen.chooseNum(10, 100)) { (n: Int) =>
      whenever(n > 10 && n < 100){
        val (rnd:Int, _) =  nonNegativeLessThanFlatmappy(n)(Simple(n))
        println(rnd)
        rnd < n
      }
    }
  }

  property("nonNegativeLessThan produces positive numbers in the right range numbers") {
    forAll(Gen.chooseNum(10, 100)) { (n: Int) =>
      whenever(n > 10 && n < 100){
        val (rnd:Int, _) =  nonNegativeLessThan(n)(Simple(n))
        rnd < n
      }
    }
  }

  property("both returns a pair of the expected rands") {
    forAll(Gen.chooseNum(1, 100)) { (n: Int) =>
      whenever(n > 1 && n < 100){
        val ((n1,n2),r) = both(nonNegativeInt,nonNegativeInt)(Simple(n))
        n1 > 0 && n2 > 0
      }
    }
  }
}


