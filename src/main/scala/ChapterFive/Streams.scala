package ChapterFive

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def drop(n: Int): Stream[A] = {
    if (n == 0)
      this
    else

      this match {
        case Empty => Empty
        case Cons(h, t) => t().drop(n - 1)
      }
  }

  def take(n: Int): Stream[A] = {
    if (n == 0)
      Empty
    else

      this match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => t() take (n - 1))
      }
  }

  def take2(n: Int): Stream[A] = {
    unfold((n, this))(tup => {
      if (tup._1 == 0)
        None
      else
        tup._2 match {
          case Cons(h, t) if n > 0 => Some(h(), (tup._1 - 1, t()))
          case Empty => None
        }
    })
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) if p(h()) => Cons(h, () => t() takeWhile (p))
      case Cons(h, t) => Empty
    }
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)
  }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case Cons(h, t) if !p(h()) => None
      case Empty => None
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, b) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))
  }

  def map2[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (f(a)) Cons(() => a, () => b) else b)
  }

  def append[B >: A](ss: => Stream[B]): Stream[B] = {
    foldRight(ss)((a, b) => Cons(() => a, () => b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
  }

  def toList(): List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  def zipWith[B](bs: Stream[B]): Stream[(A, B)] = {
    unfold(this, bs) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
      case (_, Empty) => None
      case (Empty, _) => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold(Some(this): Option[Stream[A]], Some(s2): Option[Stream[B]]) {
      case (Some(Cons(h1, t1)), Some(Cons(h2, t2))) => Some((Some(h1()), Some(h2())), (Some(t1()), Some(t2())))

      case (Some(Empty), Some(Cons(h2, t2))) => Some((None, Some(h2())), (None, Some(t2())))
      case (Some(Cons(h1, t1)), Some(Empty)) => Some((Some(h1()), None), (Some(t1()), None))

      case (None, Some(Cons(h2, t2))) => Some((None, Some(h2())), (None, Some(t2())))
      case (Some(Cons(h1, t1)), None) => Some((Some(h1()), None), (Some(t1()), None))

      case (None, Some(Empty)) => None
      case (Some(Empty), None) => None

      case (Some(Empty), Some(Empty)) => None
      case (None, None) => None
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    this.zipWith(s).foldRight(true)((tup, bool) => bool && tup._1 == tup._2)
  }

  def tails: Stream[Stream[A]] = {
    unfold(this)({
      case s@Cons(h, t) => Some(s, t())
      case Empty => None
    }).append(Stream(Stream()))
  }

//  def scanRight[B](z:B)(f:(A,A) => B): Stream[Stream[B]] = {
//    unfold(z)({
//      case s@Cons(h, t) => Some(f(), t())
//      case Empty => None
//    }).append(Stream(Stream()))
//  }


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
