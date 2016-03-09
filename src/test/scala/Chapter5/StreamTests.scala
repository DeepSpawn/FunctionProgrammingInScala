package Chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamTests extends FlatSpec with Matchers{

  "toList" should "convert a stream to a list" in {
    ChapterFive.Stream(1,2,3,4,5).toList shouldEqual List(1,2,3,4,5)
  }

  "take" should "should return a stream of the first N elements" in {
    ChapterFive.Stream(1,2,3,4,5).take(3).toList shouldEqual List(1,2,3)
    ChapterFive.Stream(1,2,3,4,5).take(1).toList shouldEqual List(1)
    ChapterFive.Stream(1,2,3,4,5).take(0).toList shouldEqual List()
    ChapterFive.Stream(1,2,3,4,5).take(6).toList shouldEqual List(1,2,3,4,5)
  }

  "takeWhile" should "convert return a stream of the consecutive elements where the predicate is true" in {
    ChapterFive.Stream(1,2,3,4,5).takeWhile(_ != 0).toList shouldEqual List(1,2,3,4,5)
    ChapterFive.Stream(2,4,6,7,8).takeWhile(_ % 2 == 0).toList shouldEqual List(2,4,6)
    ChapterFive.Stream(1,2,3,4,5).takeWhile(_ % 2 == 0).toList shouldEqual List()
  }

  "takeWhile2" should "convert return a stream of the consecutive elements where the predicate is true" in {
    ChapterFive.Stream(1,2,3,4,5).takeWhile2(_ != 0).toList shouldEqual List(1,2,3,4,5)
    ChapterFive.Stream(2,4,6,7,8).takeWhile2(_ % 2 == 0).toList shouldEqual List(2,4,6)
    ChapterFive.Stream(1,2,3,4,5).takeWhile2(_ % 2 == 0).toList shouldEqual List()
  }

  "forall" should "returns true if the predicate holds" in {
    ChapterFive.Stream(1,2,3,4,5).forAll(_ % 2 == 0) shouldEqual false
    ChapterFive.Stream(2,4,6,8).forAll(_ % 2 == 0) shouldEqual true
  }

  "headOption" should "returns the first element if present" in {
    ChapterFive.Stream(1, ()=>{throw new RuntimeException("not lazy"); 2},3,4,5).headOption shouldEqual Some(1)
    ChapterFive.Stream().headOption shouldEqual None
  }

  "append" should "returns one stream appended to the second" in {
    ChapterFive.Stream(1,2,3,4,5).append(ChapterFive.Stream(6,7,8,9,10)).toList() shouldEqual List(1,2,3,4,5,6,7,8,9,10)
    ChapterFive.Stream(1,2,3,4,5).append(ChapterFive.Stream()).toList() shouldEqual List(1,2,3,4,5)
    ChapterFive.Stream().append(ChapterFive.Stream(6,7,8,9,10)).toList() shouldEqual List(6,7,8,9,10)

  }

  "flatmap" should "returns one stream of the results" in {
    ChapterFive.Stream(1,2,3,4,5).flatMap(i => ChapterFive.Stream(i,i)).toList() shouldEqual List(1,1,2,2,3,3,4,4,5,5)
  }

  "ones2" should "return a stream of ones" in {
    ChapterFive.Stream.ones2.take(5).toList() shouldEqual List(1,1,1,1,1)
  }

  "constant" should "return a stream of the given value" in {
    ChapterFive.Stream.constant(3).take(3).toList()shouldEqual List(3,3,3)
  }

  "constant2" should "return a stream of the given value" in {
    ChapterFive.Stream.constant2(3).take(3).toList()shouldEqual List(3,3,3)
  }

  "from" should "return a stream starting from the given value" in {
    ChapterFive.Stream.from(1).take(3).toList()shouldEqual List(1,2,3)
  }

  "from2" should "return a stream starting from the given value" in {
    ChapterFive.Stream.from2(1).take(3).toList()shouldEqual List(1,2,3)
  }

  "fibs" should "return a stream of the fibbonaci sequence" in {
    ChapterFive.Stream.fibs().take(6).toList()shouldEqual List(0,1,1,2,3,5)
  }

  "fibs2" should "return a stream of the fibbonaci sequence" in {
    ChapterFive.Stream.fibs2().take(6).toList()shouldEqual List(0,1,1,2,3,5)
  }

  "unfold" should "return a stream generated from the supplied unfold function" in {
    ChapterFive.Stream.unfold(1)(n => if(n < 5) Some((n+1,n+1)) else None).toList() shouldEqual List(2,3,4,5)
  }







}
