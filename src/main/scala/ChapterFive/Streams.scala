package ChapterFive

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    if(n==0)
      return Empty
    else

    this match {
      case Empty => return Empty
      case Cons(h,t) => Cons(h, () => t()take(n-1))
    }
  }

  def drop(n: Int): Stream[A] = sys.error("todo")

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h,t) if p(h()) => Cons(h,() => t()takeWhile(p))
      case Cons(h,t) => Empty
    }
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Empty : Stream[A])((a,b) => if(p(a)) Cons(() => a, ()=> b) else Empty)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a,b) => p(a) && b)
  }

  def headOption: Option[A] = {
    foldRight(None:Option[A])((a,b) => Some(a))
  }

  def map[B](f: A => B):Stream[B] = {
    foldRight(Empty:Stream[B])((a,b) => Cons(() => f(a), () => b))
  }

  def filter(f: A => Boolean):Stream[A] = {
    foldRight(Empty : Stream[A])((a,b) => if(f(a)) Cons(() => a, ()=> b) else b)
  }

  def append[B >: A](ss : => Stream[B]): Stream[B] = {
    foldRight(ss)((a,b) => Cons(() => a, () => b))
  }

  def flatMap[B](f: A => Stream[B]):Stream[B] = {
    foldRight(Empty:Stream[B])((a,b) => f(a).append(b))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList(): List[A] = {
    this match {
      case Empty => Nil
      case Cons(h,t) => h() :: t().toList
    }
  }



}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    Cons(() => a, () => constant(a))


  def from(n: Int): Stream[Int] =
    Cons(() => n, () => from(n + 1))

  def fibs(): Stream[Int] = {
    def fib(prev:Int, current:Int):Stream[Int] = {
      Cons(() => prev + current, () => fib(current, prev + current))
    }

    Cons(() => 0, () => Cons(() => 1, () => fib(0,1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map(tup => Cons(() => tup._1, () => unfold(tup._2)(f)))
      .getOrElse(Empty: Stream[A])
  }

  val ones2: Stream[Int] = unfold(1)(n => Some(n,n))

  def constant2[A](a: A): Stream[A] = unfold(a)(a => Some(a,a))

  def from2(n: Int): Stream[Int] = unfold(n)(n => Some(n,n+1))

  def fibs2(): Stream[Int] = unfold((0,1))(tup => Some(tup._1, (tup._2, tup._1 + tup._2)))


}
