package ChapterTwo

/**
  * Created by gtaylor on 19/02/2016.
  */
object Exercises {

  //Exercise 2.1
  def fib(n: Int): Int = {
    if (n == 0) return 0

    @annotation.tailrec
    def go(iter:Int, prevFib:Int, curFib:Int): Int =
      if(iter == n) curFib
      else go(iter + 1, curFib, prevFib+curFib)

    go(1, 0, 1)
  }


}
