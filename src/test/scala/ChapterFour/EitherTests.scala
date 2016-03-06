package ChapterFour

import org.scalatest.{Matchers, FlatSpec}

class EitherTests extends FlatSpec with Matchers {

  "map" should "only map across right either" in {
    Right(1).map(_ + 1)  shouldEqual Right(2)
    (Left("error string"):Either[String,Int]).map(_ + 1)  shouldEqual Left("error string")
  }

  "flatmap" should "only map across right either" in {
    (Right(1):Either[String,Int]).flatMap(a => Right(a + 1)) shouldEqual Right(2)
    (Left("error string"):Either[String,Int]).flatMap(a => Right(a + 1))  shouldEqual Left("error string")
  }

  "orElse" should "return the value for a right, else the given val" in {
    Right(1).orElse(Right(0)) shouldEqual Right(1)
    (Left("error string"):Either[String,Int]).orElse(Right(0)) shouldEqual Right(0)
  }

  "map2" should "map to a right when both values are right" in {
    Right(1).map2(Right(2))(_ + _) shouldEqual Right(3)
    (Left("error string"):Either[String,Int]).map2(Right(2))(_ + _) shouldEqual Left("error string")
    (Right(2):Either[String,Int]).map2(Left("new error string"))(_ + _) shouldEqual Left("new error string")
  }

  "sequence" should "return a list of values if called on a list of some options" in {
    Either.sequence(List(Right(1), Right(2), Right(3))) shouldEqual Right(List(1, 2, 3))
    Either.sequence(List(Right(1), Left("e"), Right(2))) shouldEqual Left("e")
    Either.sequence(List(Right(1), Right(2), Left("e"))) shouldEqual Left("e")
  }

  "traverse" should "return a list of values if called on a list of some options" in {
    Either.traverse(List(1,2,3))(Right(_)) shouldEqual Right(List(1,2,3))
    Either.traverse(List(1,2,3))(a => if(a % 2 == 0) Right(a) else Left("odd")) shouldEqual Left("odd")
  }


}
