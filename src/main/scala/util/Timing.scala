package util

trait Timing {
  this: Logging =>
  def timed[T](toTime: => T): T = {
    val start = System.nanoTime()
    val res = toTime
    val end = System.nanoTime
    println(s"${(end - start)/(1000 * 1000)} ms needed")
    res
  }
}
