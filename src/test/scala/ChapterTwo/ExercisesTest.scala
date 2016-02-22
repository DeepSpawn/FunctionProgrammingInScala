package ChapterTwo

import ChapterTwo.Exercises.fib
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


}
