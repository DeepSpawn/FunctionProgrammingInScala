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

  //  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  //    rng => {
  //      val (a, rng2) = s(rng)
  //      (f(a), rng2)
  //    }


  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)((a: A) => (rng: RNG) => (f(a), rng))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)((a: A) => flatMap(rb)((b: B) => (rng: RNG) => (f(a, b), rng)))

  //  {
  //    rng => {
  //      val (a, rng1) = ra(rng)
  //      val (b, rng2) = rb(rng1)
  //      (f(a, b), rng2)
  //    }
  //  }

  //  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  //    rng => {
  //      val (a, rng1) = ra(rng)
  //      val (b, rng2) = rb(rng1)
  //      (f(a, b), rng2)
  //    }
  //  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs match {
      case h :: tail => map2(h, sequence(tail))((a, b) => a :: b: List[A])
      case nil => rng => (List.empty: List[A], rng)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rnd => {
      val (num, r1) = f(rnd)
      g(num)(r1)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }


  def nonNegativeLessThanFlatmappy(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)((i: Int) => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        rng => (mod, rng)
      else {
        nonNegativeLessThanFlatmappy(n)
      }
    })
  }

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
  //      if (count <= 0) return (List.empty, rng)
  //      var acc: List[Int] = List()
  //      var rndState = rng
  //      for (n <- 1 until count) {
  //        val (i, nextRng) = rndState.nextInt
  //        acc = i :: acc
  //        rndState = nextRng
  //      }
  //      (acc, rndState)
  //    }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)((rng: RNG) => rng.nextInt))(rng)
  }

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    flatMap((a: A) => State((s: S) => (f(a), s)))
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap((a: A) =>
      sb.flatMap((b: B) =>
        State[S, C]((s: S) => (f(a, b), s))))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B]((s1: S) => {
      val (a: A, s2: S) = run(s1)
      f(a: A).run(s2)
    })
}

//Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
//Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
//Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
//A machine that’s out of candy ignores all inputs.

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

}

object State {
  type Rand[A] = State[RNG, A]
  type CandyMachine = State[Machine, (Int, Int)]

  def simulateMachine(inputs: List[Input]): CandyMachine = {
    State[Machine, (Int, Int)]((m: Machine) => {
      var machine: Machine = m
      var output: ((Int, Int), Machine) = inspectMachine().run(m)
      for (input <- inputs) {
        output = run(input, machine).run(machine)
        machine = output._2
      }
      output
    })
  }


  def run(input:Input, machine: Machine): CandyMachine = {
    input match {
      case Coin => State[Machine, (Int, Int)] ((m:Machine) => inspectMachine().run(insertCoin(m)))
      case Turn => State[Machine, (Int, Int)] ((m:Machine) => inspectMachine().run(turnKnob(machine)))
    }


  }

  def insertCoin(m: Machine): Machine = {
    m match {
      //A machine that’s out of candy ignores all inputs.
      case Machine(_, 0, _) => m
      //Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
      case Machine(true, candies, coins) if candies > 0 => Machine(false, candies, coins + 1)
      //Inserting a coin into an unlocked machine does nothing.
      case Machine(false, _, _) => m
    }
  }

  def turnKnob(m: Machine): Machine = {
    m match {
      //A machine that’s out of candy ignores all inputs.
      case Machine(_, 0, _) => m
      //Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
      case Machine(false, candies, coins) if candies > 0 => Machine(true, candies - 1, coins)
      //Turning the knob on a locked machine does nothing.
      case Machine(true, _, _) => m
    }
  }

  def inspectMachine() : CandyMachine = {
  State[Machine, (Int, Int)] {
    case machine@Machine(_, candy, coins) => ((candy, coins), machine)
  }
  }

}