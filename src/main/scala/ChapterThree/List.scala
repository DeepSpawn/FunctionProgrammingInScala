package ChapterThree

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.


  /* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
  which may be `Nil` or another `Cons`.
   */

  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((h, t) => Cons(h, t))


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Cons(h, Nil)
      case Cons(_, t) => Cons(h, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(i: Int, ll: List[A]): List[A] = {
      if (i == n) return ll
      go(i + 1, tail(ll))
    }
    go(0, l)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      //note the use of a guard statement that means it wont match unless the guard returns true
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(h, t) if t == Nil => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((a, b) => 1 + b)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l,Nil:List[B])((a,b) => Cons(f(a),b))
  }

  def concatenate[A](ll: List[List[A]]): List[A] = {
    foldRight(ll, Nil: List[A])(append)
  }

  def addOne(l: List[Int]): List[Int] = {
    foldRight(l,Nil:List[Int])((h,t) => Cons(h+1,t))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l,Nil:List[String])((h,t) => Cons(h.toString,t))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((h,t) => if(f(h)) Cons(h,t) else t)
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(i => if(f(i)) List(i) else Nil)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concatenate(map(as)(f))
  }

  def addElem(as:List[Int],bs:List[Int]) : List[Int] = {

    (as,bs) match {
      case (aas,Nil) => aas
      case (Nil,bbs) => bbs
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2, addElem(t1,t2))
    }
  }

  def zipWith[A](as:List[A], bs:List[A])(f:(A,A) => A) : List[A] = {
    (as,bs) match {
      case (aas,Nil) => aas
      case (Nil,bbs) => bbs
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup,sub) match {
      case (_,Nil) => return true
      case (Cons(h1,t1),Cons(h2,t2)) if h1==h2 => hasSubsequence(t1,t2)
      case (Cons(h1,t1),Cons(h2,t2)) => hasSubsequence(t1, sub)
      case (Nil, _) => return false
    }
  }


}
