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



}
