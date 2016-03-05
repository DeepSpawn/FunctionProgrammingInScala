package ChapterFour

import org.scalatest.{Matchers, FlatSpec}
import ChapterFour.{Some,None,Option =>  myOption}

class OptionTests extends FlatSpec with Matchers {

  "map" should "only map some options" in {
    Some(1).map(_ + 1)  shouldEqual Some(2)
    (None :Option[Int]).map(_ + 1)  shouldEqual None
  }

  "filter" should "should only return somes when the predicate is true" in {
    Some(1).filter(_ % 2 == 0) shouldEqual None
    Some(2).filter(_ % 2 == 0) shouldEqual Some(2)
    (None :Option[Int]).filter(_ % 2 == 0)  shouldEqual None
  }

  "getOrElse" should "only get the contained val else return the given val" in {
    Some(1).getOrElse(0) shouldEqual 1
    (None :Option[Int]).getOrElse(2)  shouldEqual 2
  }

  "orElse" should "should return a some of the held val else a some of the given val" in {
    Some(1).orElse(Some(0)) shouldEqual Some(1)
    (None :Option[Int]).orElse(Some(0))  shouldEqual Some(0)
  }

  "flatmap" should "only map some options" in {
    Some(1).flatMap(a => Some(a.toString))  shouldEqual Some("1")
    (None :Option[Int]).flatMap(a => Some(a.toString))  shouldEqual (None:Option[String])
  }

  "variance" should "return the variance for non empty sequences" in {
    ChapterFour.Option.variance(List(3,4,7,10))  shouldEqual Some(7.5)
    ChapterFour.Option.variance(List())  shouldEqual None
  }

  "sequence" should "return a list of values if called on a list of some options" in {
    ChapterFour.Option.sequence(List(Some(1),Some(2),Some(3))) shouldEqual Some(List(1,2,3))
    ChapterFour.Option.sequence(List(Some(1),None,Some(2))) shouldEqual None
    ChapterFour.Option.sequence(List(Some(1),Some(2),None)) shouldEqual None
  }

  "sequence2" should "return a list of values if called on a list of some options" in {
    ChapterFour.Option.sequence2(List(Some(1),Some(2),Some(3))) shouldEqual Some(List(1,2,3))
    ChapterFour.Option.sequence2(List(Some(1),None,Some(2))) shouldEqual None
    ChapterFour.Option.sequence2(List(Some(1),Some(2),None)) shouldEqual None
  }

  "traverse" should "return a list of values if called on a list of some options" in {
    ChapterFour.Option.traverse(List(1,2,3))(Some(_)) shouldEqual Some(List(1,2,3))
    ChapterFour.Option.traverse(List(1,2,3))(a => if(a % 2 == 0) Some(a) else None) shouldEqual None
  }



}
