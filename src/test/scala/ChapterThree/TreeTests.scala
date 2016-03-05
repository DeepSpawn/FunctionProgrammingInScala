package ChapterThree

import ChapterThree.Tree.{Branch, Leaf, Tree}
import org.scalatest._

/**
  * Created by gtaylor on 5/03/2016.
  */
class TreeTests extends FlatSpec with Matchers {

  "size" should "return the number of nodes in the tree" in {
    Tree.Tree.size(Leaf("a")) shouldEqual 1
    Tree.Tree.size(Branch(Leaf("a"),Leaf("b"))) shouldEqual 2
    Tree.Tree.size(Branch(Branch(Branch(Branch(Leaf("a"),Leaf("b")),Leaf("c")),Leaf("d")),Leaf("e"))) shouldEqual 5
  }

  "max" should "return the maximum value in the tree" in {
    Tree.Tree.maximum(Leaf(1)) shouldEqual 1
    Tree.Tree.maximum(Branch(Leaf(1),Leaf(2))) shouldEqual 2
    Tree.Tree.maximum(Branch(Branch(Branch(Branch(Leaf(1),Leaf(2)),Leaf(5)),Leaf(4)),Leaf(3))) shouldEqual 5
  }

  "depth" should "return the maximum path depth to a leaf node" in {
    Tree.Tree.depth(Leaf(1)) shouldEqual 1
    Tree.Tree.depth(Branch(Leaf(1),Leaf(2))) shouldEqual 2
    Tree.Tree.depth(Branch(Branch(Branch(Branch(Leaf(1),Leaf(2)),Leaf(5)),Leaf(4)),Leaf(3))) shouldEqual 5
  }

  "map" should "return apply the function to each leaf in the tree" in {
    Tree.Tree.map(Leaf(1))(_ + 1) shouldEqual Leaf(2)
    Tree.Tree.map(Branch(Leaf(1),Leaf(2)))(_.toString) shouldEqual Branch(Leaf("1"),Leaf("2"))
    Tree.Tree.map(Branch(Branch(Branch(Branch(Leaf(1),Leaf(2)),Leaf(5)),Leaf(4)),Leaf(3)))(_ + 1) shouldEqual
      Branch(Branch(Branch(Branch(Leaf(2),Leaf(3)),Leaf(6)),Leaf(5)),Leaf(4))
  }

  "size2" should "return the number of nodes in the tree" in {
    Tree.Tree.size2(Leaf("a")) shouldEqual 1
    Tree.Tree.size2(Branch(Leaf("a"),Leaf("b"))) shouldEqual 2
    Tree.Tree.size2(Branch(Branch(Branch(Branch(Leaf("a"),Leaf("b")),Leaf("c")),Leaf("d")),Leaf("e"))) shouldEqual 5
  }

  "max2" should "return the maximum value in the tree" in {
    Tree.Tree.maximum2(Leaf(1)) shouldEqual 1
    Tree.Tree.maximum2(Branch(Leaf(1),Leaf(2))) shouldEqual 2
    Tree.Tree.maximum2(Branch(Branch(Branch(Branch(Leaf(1),Leaf(2)),Leaf(5)),Leaf(4)),Leaf(3))) shouldEqual 5
  }

  "depth2" should "return the maximum path depth to a leaf node" in {
    Tree.Tree.depth2(Leaf(1)) shouldEqual 1
    Tree.Tree.depth2(Branch(Leaf(1),Leaf(2))) shouldEqual 2
    Tree.Tree.depth2(Branch(Branch(Branch(Branch(Leaf(1),Leaf(2)),Leaf(5)),Leaf(4)),Leaf(3))) shouldEqual 5
  }

  "map2" should "return apply the function to each leaf in the tree" in {
    Tree.Tree.map2(Leaf(1))(_ + 1) shouldEqual Leaf(2)
    Tree.Tree.map2(Branch(Leaf(1),Leaf(2)))(_.toString) shouldEqual Branch(Leaf("1"),Leaf("2"))
    Tree.Tree.map2(Branch(Branch(Branch(Branch(Leaf(1),Leaf(2)),Leaf(5)),Leaf(4)),Leaf(3)))(_ + 1) shouldEqual
      Branch(Branch(Branch(Branch(Leaf(2),Leaf(3)),Leaf(6)),Leaf(5)),Leaf(4))
  }

}

