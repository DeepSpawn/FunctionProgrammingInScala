package parallel

import java.util.concurrent._
import parallel.Par.TimedResult

import language.implicitConversions


object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)


      timedExecution(() => af.get())
      UnitFuture(f(af.get, bf.get))
    }
  }

//  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C, timeout: Long, unit: TimeUnit): Par[C] = {
//    (es: ExecutorService) => {
//      val af = a(es)
//      val bf = b(es)
//      val (t1, r1) = timedExecution(() => af.get(timeout, unit))
//      val (_, r2) = timedExecution(() => bf.get(timeout - t1, unit))
//      UnitFuture(f(r1, r2))
//    }
//  }


  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = ???

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    def consResults(p: Par[A], pl: Par[List[A]]): Par[List[A]] = {
      map2(p: Par[A], pl: Par[List[A]])((a: A, pl: List[A]) => a :: pl)
    }

    ps.foldRight(unit(List.empty: List[A]))(consResults)


  }

  private case class TimedFuture[TimedResult[_], B](delegate: Future[B]) extends Future[Par.TimedResult[B]] {
    override def isCancelled: Boolean = delegate.isCancelled

    override def get(timeout: Long, unit: TimeUnit): Par.TimedResult[B] = {
      timedExecution(() => delegate.get(timeout, unit))
    }

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = delegate.cancel(mayInterruptIfRunning)

    override def isDone: Boolean = delegate.isDone

    override def get(): Par.TimedResult[B] = {
      timedExecution(() => delegate.get())
    }
  }

  private def timedExecution[A](f: () => A): TimedResult[A] = {
    val startedAt = System.currentTimeMillis()
    val a = f.apply()
    val finishedAt = System.currentTimeMillis()
    TimedResult(finishedAt - startedAt, a)
  }

  private case class TimedResult[A](executionTime: Long, a: A)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }


}



