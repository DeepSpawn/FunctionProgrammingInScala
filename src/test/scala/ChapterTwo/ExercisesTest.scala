package ChapterTwo

import ChapterTwo.Exercises._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ShouldMatchers, PropSpec}

class ExercisesTest extends PropSpec with PropertyChecks with ShouldMatchers {

  //2.1
  property("fib value is equal to the sum of its predecessors") {
    forAll { (n: Int) =>
      whenever(n >= 3 && n != Integer.MAX_VALUE) {
        fib(n) should be === fib(n - 2) + fib(n - 1)
      }
    }
  }

  val genIntArray = Gen.containerOf[Array, Int](
    Gen.chooseNum(Int.MinValue, Int.MaxValue)
  )

  val genSortedIntArray = genIntArray.map(_.sorted)

  //2.2
  property("returns true for sorted int arrays") {
    forAll(genSortedIntArray) {
      (a) => whenever(a.length > 2 && !(a.sorted sameElements a)) {
        isSorted(a, (n: Int, m: Int) => n <= m) should be === true
      }
    }
  }

  property("returns false for nonsorted int arrays") {
    forAll(genIntArray) {
      (a) => whenever(a.length > 2) {
        isSorted(a, (n: Int, m: Int) => n <= m) should be === false
      }
    }
  }

}
