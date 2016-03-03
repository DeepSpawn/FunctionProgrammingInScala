package ChapterThree

import org.scalatest._
import ChapterThree.{List => MyList}

/**
  * Created by gtaylor on 24/02/2016.
  */
class ListTests extends FlatSpec with Matchers {


  "tail" should "return all elements but the first" in {
    ChapterThree.List.tail(MyList(1, 2, 3)) shouldEqual MyList(2, 3)
  }

  "drop 3" should "drop the first three elements of the list" in {
    ChapterThree.List.drop(MyList(1, 2, 3, 4, 5),3) shouldEqual MyList(4, 5)
  }

  "drop even" should "drop the leading even numbers of the list" in {
    ChapterThree.List.dropWhile(MyList(2,4,6,1,2,3))(_ % 2 == 0) shouldEqual MyList(1,2,3)
  }

  "init" should "return a new list with all but the last element of the original" in {
    ChapterThree.List.init(MyList(1,2,3,4)) shouldEqual MyList(1,2,3)
  }

  "foldright" should "return the given list when called with Cons and Nil" in {
    ChapterThree.List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) shouldEqual MyList(1,2,3)
  }

  "length" should "return the length of the list" in {
    ChapterThree.List.length(List(1,2,3)) shouldEqual 3
  }

  "append" should "append one list to the other" in {
    ChapterThree.List.append(List(1,2,3), List(4,5,6)) shouldEqual List(1,2,3,4,5,6)
  }

  "append2" should "append one list to the other" in {
    ChapterThree.List.append2(List(1,2,3), List(4,5,6)) shouldEqual List(1,2,3,4,5,6)
  }

  "sum" should "should sum the list" in {
    ChapterThree.List.sum(List(1,2,3)) shouldEqual 6
  }

  "sum2" should "should sum the list" in {
    ChapterThree.List.sum2(List(1,2,3)) shouldEqual 6
  }

  "product" should "should calculate the product of the list" in {
    ChapterThree.List.product(List(1,2,3,4)) shouldEqual 24
  }

  "product2" should "should calculate the product of the list" in {
    ChapterThree.List.product2(List(1,2,3,4)) shouldEqual 24
  }

  "set head" should "should replace the head of the list" in {
    ChapterThree.List.setHead(List(0,2,3),1) shouldEqual List(1,2,3)
  }

  "concatenate" should "should combine all the lists to a single list" in {
    ChapterThree.List.concatenate(List(List(1,2),List(3,4),List(5,6))) shouldEqual List(1,2,3,4,5,6)
  }

  "addOne" should "should incremnt each element" in {
    ChapterThree.List.addOne(List(1,2,3)) shouldEqual List(2,3,4)
  }

  "doubleToString" should "convert each double to a string" in {
    ChapterThree.List.doubleToString(List(1.0,2.0,3.0)) shouldEqual List("1.0","2.0","3.0")
  }

  "filter" should "remove items that dont match the predicate" in {
    ChapterThree.List.filter(List(1,2,3,4))(_ % 2 == 0) shouldEqual List(2,4)
  }

  "filter2" should "remove items that dont match the predicate" in {
    ChapterThree.List.filter(List(1,2,3,4))(_ % 2 == 0) shouldEqual List(2,4)
  }

  "flatmap" should "remove flatmap correctly" in {
    ChapterThree.List.flatMap(List(1,2,3))(i => List(i,i)) shouldEqual List(1,1,2,2,3,3)
  }

  "addElem" should "remove add corresponding elements together" in {
    ChapterThree.List.addElem(List(1,2,3),List(4,5,6)) shouldEqual List(5,7,9)
  }

  "zipWith" should "zip the lists together" in {
    ChapterThree.List.zipWith(List(1,2,3),List(4,5,6))(_ * _) shouldEqual List(4,10,18)
  }




}
