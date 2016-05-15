package ChapterSix

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs match {
      case nil => rng => (List.empty: List[A], rng)
      case h :: tail => map2(h, sequence(tail))((a, b) => a :: b: List[A])
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  @annotation.tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    if (i == Int.MinValue) nonNegativeInt(nextRng) else (i, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = rng.nextInt
    (i.toDouble, nextRng)
  }

  def doubleMap(rng: RNG): Rand[Double] = {
    map(rng => rng.nextInt)(i => i.toDouble)
  }


  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (r, nextRng2) = double(nextRng)
    ((i, r), nextRng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (rnds, nextRng) = intDouble(rng)
    (rnds.swap, nextRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, nextRng1) = double(rng)
    val (d2, nextRng2) = double(nextRng1)
    val (d3, nextRng3) = double(nextRng2)
    ((d1, d2, d3), nextRng3)
  }

//  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  //    if (count <= 0) return (List.empty, rng)
  //    var acc: List[Int] = List()
  //    var rndState = rng
  //    for (n <- 1 until count) {
  //      val (i, nextRng) = rndState.nextInt
  //      acc = i :: acc
  //      rndState = nextRng
  //    }
  //    (acc, rndState)
  //  }

  def ints(count: Int)(rng: RNG): Rand[List[Int]] = sequence(List.fill(count)((rng:RNG) => rng.nextInt))


}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}